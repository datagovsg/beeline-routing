package sg.beeline.web

import java.time._

import akka.actor.{ActorRef, ActorSystem}
import akka.http.scaladsl.Http
import akka.http.scaladsl.marshalling.Marshal
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers.{Authorization, OAuth2BearerToken, RawHeader}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.unmarshalling.Unmarshal
import akka.stream.ActorMaterializer
import akka.util.Timeout
import io.circe.Json
import pdi.jwt.{JwtAlgorithm, JwtCirce, JwtClaim, JwtOptions}
import sg.beeline.problem._
import sg.beeline.ruinrecreate.BeelineRecreateSettings
import sg.beeline.util.{ExpiringCache, Util}
import sg.beeline.util.Util.Point
import sg.beeline.web.Auth.User

import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

trait E2EAuthSettings {
  def googleMapsApiKey: String
  def authVerificationKey: String
  def beelineServer: String
}

object DefaultE2EAuthSettings extends E2EAuthSettings {
  override val googleMapsApiKey = sys.env("GOOGLE_MAPS_API_KEY")
  override val authVerificationKey = sys.env("AUTH0_SECRET")
  override val beelineServer = sys.env("BEELINE_SERVER")
}

class E2ESuggestion(routingActor: ActorRef)
                   (implicit system: ActorSystem,
                    executionContext: ExecutionContext,
                    timeout: Timeout,
                    authSettings: E2EAuthSettings)
extends JsonSupport {
  implicit val materializer = ActorMaterializer()
  import objectJsonMarshallers._

  val http = Http()

  val e2eRoutes =
    path("suggestions" / IntNumber / "update") { suggestionId =>
      post {
        Auth(authSettings).authDirective {
          case User(userId) =>
            entity(as[BeelineRecreateSettings]) { recreateSettings =>
              headerValueByName("Authorization") { authorization =>
                import akka.pattern.ask

                val fut = for {
                  suggestRequest <- verifySuggestionId(authorization, suggestionId, userId, recreateSettings)
                  routes <- (routingActor ? suggestRequest).mapTo[List[Route2]]
                  _ <- {
                    if (routes.isEmpty) {
                      Future.failed(new RuntimeException("No suitable routes were found"))
                    } else {
                      val bestRoute = routes.maxBy(_.requests.size)

                      for {
                        workingDay <- getNextWorkingDay()
                        googleMapsTimings <- {
                          getGoogleMapsTimings(bestRoute, workingDay.toLocalDate, suggestRequest.time.toInt)
                        }
                        result <- pushToServer(suggestionId, googleMapsTimings, bestRoute)
                      } yield result
                    }
                  }
                } yield ()

                fut onComplete {
                  case Failure(e) =>
                    notifyServerOfError(suggestionId)
                    e.printStackTrace(System.err)
                    println(fut)
                  case _ => ()
                }

                complete(StatusCodes.Accepted, "Job Queued")
              }

            } ~ complete(StatusCodes.BadRequest)
          case _ =>
            complete(StatusCodes.Forbidden)
        }
      }
    }

  /**
    * Resolves to nothing if the suggestion belongs to the user.
    * Otherwise throws a failed future
    * @param authorization
    * @param suggestionId
    * @return
    */
  private def verifySuggestionId(authorization: String,
                                 suggestionId: Int,
                                 authUserId: Int,
                                 recreateSettings: BeelineRecreateSettings): Future[SuggestRequest] = {
    for {
      resp <- http.singleRequest(HttpRequest(
        uri = s"${authSettings.beelineServer}/suggestions/${suggestionId}",
        headers = List(new RawHeader("Authorization", authorization))))
      json <- Unmarshal(resp._3).to[Json]
    } yield {
      val cur = json.hcursor
      // Extract the suggestion parameters
      val suggestionEither = for {
        boardLng <- cur.downField("board").downField("coordinates").downN(0).as[Double]
        boardLat <- cur.downField("board").downField("coordinates").downN(1).as[Double]
        alightLng <- cur.downField("alight").downField("coordinates").downN(0).as[Double]
        alightLat <- cur.downField("alight").downField("coordinates").downN(1).as[Double]
        time <- cur.downField("time").as[Int]
        daysMask <- cur.downField("daysMask").as[Int]
        userId <- cur.downField("userId").as[Int]
      } yield {
        require(userId == authUserId)
        SuggestRequest(
          startLat = boardLat,
          startLng = boardLng,
          endLat = alightLat,
          endLng = alightLng,
          time = time,
          settings = BeelineRecreateSettings(
            maxDetourMinutes = recreateSettings.maxDetourMinutes,
            startClusterRadius = recreateSettings.startClusterRadius,
            startWalkingDistance = recreateSettings.startWalkingDistance,
            endClusterRadius = recreateSettings.endClusterRadius,
            endWalkingDistance = recreateSettings.endWalkingDistance,
            timeAllowance = recreateSettings.timeAllowance,
            daysOfWeek = daysMask
          )
        )
      }

      suggestionEither match {
        case Right(suggestion) => suggestion
        case Left(exc) => throw exc
      }
    }
  }

  private def fetchNextWorkingDay: Future[ZonedDateTime] = {
    case class DateEntry(date: String, summary: String)

    val publicHolidaysFut = for {
      response <- http.singleRequest(HttpRequest(
        uri=s"${authSettings.beelineServer}/publicHolidays"
      ))
      listOfPublicHolidays <- response match {
        case HttpResponse(StatusCodes.OK, _, entity, _) =>
          import _root_.io.circe.generic.extras.semiauto._
          implicit val decoder = deriveDecoder[DateEntry]

          Unmarshal(entity).to[List[DateEntry]]
        case r =>
          throw new RuntimeException(s"Bad response ${r._1.value}")
      }
    } yield {
      listOfPublicHolidays.map({ case DateEntry(date, _) => ZonedDateTime.parse(date)}).toSet
    }

    // fetch the next weekday that is not
    publicHolidaysFut.map { phSet: Set[ZonedDateTime] =>
      @tailrec
      def tryDay(zdt: ZonedDateTime): ZonedDateTime =
        if (!(phSet contains zdt) &&
          zdt.getDayOfWeek != DayOfWeek.SATURDAY &&
          zdt.getDayOfWeek != DayOfWeek.SUNDAY) zdt
        else tryDay(zdt.plusDays(1))

      tryDay(ZonedDateTime.of(
        LocalDate.now,
        LocalTime.of(0, 0, 0),
        ZoneOffset.UTC
      ).plusDays(1))
    }
  }

  private val getNextWorkingDay = {
    import scala.concurrent.duration._
    ExpiringCache(1 hour)(fetchNextWorkingDay)
  }

  private def getP2PGoogleMapsTravelTime(from: Point, to: Point,
                                         workingDay: LocalDate,
                                         arrivalTime: Int): Future[Int] = {
    case class QDuration(value: Int)
    case class QLeg(duration: QDuration)
    case class QRoute(legs: List[QLeg])
    case class QResult(routes: List[QRoute])

    import _root_.io.circe.generic.extras.auto._

    for {
      response <- http.singleRequest(HttpRequest(
        uri=Uri("https://maps.googleapis.com/maps/api/directions/json")
          .withQuery(Uri.Query(
            "origin" -> s"${from._2},${from._1}",
            "destination" -> s"${to._2},${to._1}",
            "mode" -> "driving",
            "arrival_time" ->
              // Singapore time
              ZonedDateTime.of(workingDay, LocalTime.ofSecondOfDay(arrivalTime / 1000), ZoneId.of("Asia/Singapore"))
                .toEpochSecond
                .toString,
            "key" -> authSettings.googleMapsApiKey
          ))
      ))
      s <- Unmarshal(response._3).to[Json]
    } yield {
      // Google API provides times in seconds
      // We convert them to milliseconds for consistency
      val qresult = s.as[QResult] match {
        case Right(q) => q
        case Left(exc) => throw exc
      }
      qresult.routes.head.legs.map(_.duration.value).sum * 1000
    }
  }

  private def getGoogleMapsTimings(route: Route2, workingDate: LocalDate, arrivalTime: Int): Future[List[Int]] = {
    // obtain a list of stops
    // assume a dwell time of 1 minute at each stop
    val stops = route.stops.sliding(2).foldRight( Future(List[Int](arrivalTime)) )({
      case (List(from, to), prev) =>
        for {
          arrivalTimes <- prev
          travelTime <- getP2PGoogleMapsTravelTime(from.coordinates, to.coordinates, workingDate, arrivalTimes.head)
        } yield {
          val arrivalTime = arrivalTimes.head - 60000 - travelTime
          arrivalTime :: arrivalTimes
        } // 1-minute dwell time imputed...
    })

    stops
  }

  private def notifyServerOfError(suggestionId: Int): Future[Unit] = {
    import io.circe.syntax._

    for {
      entity <- {
        Marshal(false.asJson).to[RequestEntity]
      }
      response <- http.singleRequest(
        HttpRequest(
          uri=Uri(s"${authSettings.beelineServer}/suggestions/${suggestionId}/suggested_routes"),
          headers = List(superadminHeader()),
          method = HttpMethods.POST,
          entity = entity
        )
      )
      _ = response._3.discardBytes()
    } yield response match {
      case HttpResponse(StatusCodes.OK, _, _, _) => Success(())
      case r => Failure(new RuntimeException(s"Notification of route suggestion failure returned ${r.status.value}"))
    }
  }

  private def pushToServer(suggestionId: Int,
                           googleMapsTimings: List[Int],
                           route: Route2) = {

    val stopToStopIdFut = syncBusStops(route)

    for {
      stopToStopId <- stopToStopIdFut
      entity <- {
        import io.circe.syntax._

        val pickups = route.pickups.map { case (stop, requests) =>
          val numRequests = requests.size
          Json.obj(
            "lat" -> Json.fromDoubleOrNull(stop.coordinates._2),
            "lng" -> Json.fromDoubleOrNull(stop.coordinates._1),
            "busStopIndex" -> Json.fromInt(stop.index),
            "stopId" -> Json.fromInt(stopToStopId(stop)),
            "description" -> stop.description.asJson,
            "numBoard" -> numRequests.asJson,
            "numAlight" -> 0.asJson
          )
        }

        val dropoffs = route.dropoffs.map { case (stop, requests) =>
          val numRequests = requests.size
          Json.obj(
            "lat" -> Json.fromDoubleOrNull(stop.coordinates._2),
            "lng" -> Json.fromDoubleOrNull(stop.coordinates._1),
            "busStopIndex" -> Json.fromInt(stop.index),
            "stopId" -> Json.fromInt(stopToStopId(stop)),
            "description" -> stop.description.asJson,
            "numBoard" -> 0.asJson,
            "numAlight" -> numRequests.asJson
          )
        }

        val submission = Json.arr((pickups ++ dropoffs, googleMapsTimings).zipped.map {
          case (stopJson, time) =>
            stopJson.mapObject(_.add("time", time.asJson))
        } : _*)

        Marshal(submission).to[RequestEntity]
      }
      response <- http.singleRequest(
        HttpRequest(
          uri=Uri(s"${authSettings.beelineServer}/suggestions/${suggestionId}/suggested_routes"),
          headers = List(superadminHeader()),
          method = HttpMethods.POST,
          entity = entity
        )
      )
    } yield response match {
      case HttpResponse(StatusCodes.OK, _, _, _) => Success(())
      case r => Failure(new RuntimeException(s"Posting of suggested route returned ${r.status.value}"))
    }
  }

  private def superadminHeader() = {
    val token = JwtCirce.encode(JwtClaim(
        content = """
              |{
              |  "role": "superadmin",
              |  "email": "routing@beeline.sg",
              |  "emailVerified": true
              |}
            """.stripMargin,
        expiration = Some(System.currentTimeMillis / 1000 + 60)
      ),
      authSettings.authVerificationKey,
      JwtAlgorithm.HS256
    )
    new RawHeader("Authorization", s"Bearer $token")
  }

  private def syncBusStops(route: Route2): Future[Map[BusStop, Int]] = {
    case class DBGeometry(coordinates: Array[Double])
    case class BusStopDB(
                        coordinates: DBGeometry,
                        description: String = "Unnamed Bus Stop",
                        id: Int
                        )

    import _root_.io.circe.generic.extras.auto._

    def dist(busStopDB: BusStopDB) = {
      import sg.beeline.util.kdtreeQuery.squaredDistance
      val xy = Util.toSVY((busStopDB.coordinates.coordinates(0), busStopDB.coordinates.coordinates(1)))

      { stop: BusStop => squaredDistance(stop.xy, xy) }
    }

    for {
      allBusStopsResponse <- http.singleRequest(HttpRequest(
        uri = s"${authSettings.beelineServer}/stops"
      ))
      allBusStops <- Unmarshal(allBusStopsResponse._3).to[List[BusStopDB]]
      stopsToIdMap <- {
        val distanceToBestMatchingStop = route.stops.map({ stop =>
          allBusStops.map(stopInDB => (stop, stopInDB, dist(stopInDB)(stop)))
            .minBy(_._3)
        })

        // Create the stops
        val stopCreationFuture = Future.traverse(distanceToBestMatchingStop)({ case (stop, dbStop, dist) =>
          if (dist < 20)
            Future(stop -> dbStop.id)
          else
            // create the stop
            for {
              postData <- {
                val json = Json.obj(
                  "road" -> Json.fromString(""),
                  "description" -> Json.fromString(stop.description + " (created by routing)"),
                  "postcode" -> Json.Null,
                  "label" -> stop.stopCode.map(Json.fromString).getOrElse(Json.Null),
                  "coordinates" -> Json.obj(
                    "type" -> Json.fromString("Point"),
                    "coordinates" -> Json.arr(
                      List(stop.coordinates._1, stop.coordinates._2).map(Json.fromDoubleOrNull): _*)
                  )
                )
                Marshal(json).to[RequestEntity]
              }
              postResponse <- http.singleRequest(HttpRequest(
                uri = s"${authSettings.beelineServer}/stops",
                method = HttpMethods.POST,
                entity = postData,
                headers = List(superadminHeader())
              ))
              newStop <- Unmarshal(postResponse._3).to[BusStopDB]
            } yield stop -> newStop.id
        })

        stopCreationFuture.map(_.toMap)
      }
    } yield stopsToIdMap
  }
}

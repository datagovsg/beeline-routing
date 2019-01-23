package sg.beeline.web

import java.time._

import akka.actor.{ActorRef, ActorSystem}
import akka.http.scaladsl.{Http, server}
import akka.http.scaladsl.marshalling.Marshal
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.unmarshalling.Unmarshal
import akka.stream.ActorMaterializer
import akka.util.Timeout
import io.circe.Json
import pdi.jwt.{JwtAlgorithm, JwtCirce, JwtClaim}
import sg.beeline.exc
import sg.beeline.exc.RoutingException
import sg.beeline.io.{BuiltIn, SuggestionsSource}
import sg.beeline.problem._
import sg.beeline.ruinrecreate.BeelineRecreateSettings
import sg.beeline.util.{ExpiringCache, Projections}
import sg.beeline.web.Auth.User
import sg.beeline.web.MapsAPIQuery.MapsQueryResult

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Try}

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

object E2ESuggestion {
  val IMPUTED_DWELL_TIME = 60000
  val SCHEDULING_OPTIMISM = 3 * 60000

  /**
    * Adjust the times using the formula:
    *
    * t_sched(0) = t_predicted(0)
    * t_sched(i) = max { t_sched(i - 1) + 1min, t_predicted(i) - 3 mins } for pickup stops
    * t_sched(i) = t_predicted(i) for dropoff stops
    *
    * The idea is that scheduled buses can always arrive a bit later than scheduled, but never earlier.
    * Therefore we should schedule a slightly earlier arrival time than what's predicted.
    *
    * However, we also need to maintain adequate spacing between the bus stops, hence the enforced 1min gap.
    *
    * @param timings
    * @return
    */
  def tweakPathTimings(timings: List[Int], numberOfPickupStops: Int): List[Int] = {
    val arrayBuffer = new ArrayBuffer[Int]()

    arrayBuffer += timings.head

    timings.zipWithIndex.tail.foreach { case (gmapPredictedTiming, index) =>
      val isPickupStop = index < numberOfPickupStops

      val newSchedTiming =
        if (isPickupStop)
          ((arrayBuffer.last + IMPUTED_DWELL_TIME) max (gmapPredictedTiming - SCHEDULING_OPTIMISM))
        else
          gmapPredictedTiming

      arrayBuffer += newSchedTiming
    }
    arrayBuffer.toList
  }
}

class E2ESuggestion(routingActor: ActorRef, suggestionsSource: SuggestionsSource)
                   (implicit system: ActorSystem,
                    executionContext: ExecutionContext,
                    timeout: Timeout,
                    authSettings: E2EAuthSettings) {
  implicit val materializer: ActorMaterializer = ActorMaterializer()

  import JsonMarshallers._
  import BeelineJsonMarshallers._
  import objectJsonMarshallers._
  import E2ESuggestion._

  val http = Http()
  val mapsAPIQuery = new MapsAPIQuery(http, materializer, executionContext)

  def triggerRouteGeneration(suggestionId: Int): server.Route = {
    Auth(authSettings).authDirective {
      case User(userId) =>
        entity(as[BeelineRecreateSettings]) { recreateSettings =>
          headerValueByName("Authorization") { authorization =>

            val trySuggest = makeSuggestRequest(suggestionId, userId, recreateSettings)
            trySuggest match {
              case Left(e) => e match {
                case StatusCodes.Unauthorized => complete(e, s"Suggestion $suggestionId is not available to user $userId")
                case _ => complete(e)
              }
              case Right((_, lastTriggerMillis)) if lastTriggerMillis.map(System.currentTimeMillis - _).exists(_ < 20.seconds.toMillis) =>
                complete(StatusCodes.TooManyRequests, s"Last trigger request made less than 20 seconds ago")
              case Right((suggestRequest, _)) => {
                Try(suggestionsSource.markTriggerTimestamp(suggestionId))
                  .recover({ case e => notifyServerOfError(suggestionId, e.getMessage) })
                scheduleRouteGeneration(suggestionId, suggestRequest)
                complete(StatusCodes.Accepted, "Job Queued")
              }
            }
          }
        } ~ complete(StatusCodes.BadRequest)
      case _ =>
        complete(StatusCodes.Forbidden)
    }
  }

  private def scheduleRouteGeneration(suggestionId: Int, suggestRequest: SuggestRequest) = {
    import akka.pattern.ask
    val fut = for {
      routes <- (routingActor ? suggestRequest).mapTo[Try[List[Route2]]].map(_.get)
      _ <- {
        if (routes.isEmpty) {
          Future.failed(exc.NoRoutesFound())
        } else {
          val bestRoute = routes.maxBy(_.requests.size)

          for {
            workingDay <- getNextWorkingDay()
            (googleMapsTimings, paths) <- {
              getGoogleMapsTimingsAndPaths(bestRoute, workingDay.toLocalDate, suggestRequest.time.toInt)
            }
            optimisticTimings = tweakPathTimings(googleMapsTimings, bestRoute.pickups.size)
            result <- pushToServer(suggestionId, optimisticTimings, paths, bestRoute)
          } yield result
        }
      }
    } yield ()

    fut onComplete {
      case Failure(e) =>
        e match {
          case re: RoutingException =>
            notifyServerOfError(suggestionId, re.reason)
          case _ =>
            notifyServerOfError(suggestionId, "internal_server_error")
        }
        println(s"Failed to generate a route for $suggestionId")
        e.printStackTrace(System.err)
      case _ => ()
    }
  }

  private def expectStatus(response: HttpResponse, message: String, status: StatusCode = StatusCodes.OK) =
    if (response.status.value == status.value) Future.unit
    else Future.failed(new RuntimeException(s"${message}. Got a status ${response.status.value}"))

  /**
    * @param suggestionId the suggestion id
    * @param authUserId the user id looking up the suggestion
    * @return a SuggestRequest with corresponding last trigger time, if any
    */
  private def makeSuggestRequest(suggestionId: Int,
                                 authUserId: Int,
                                 recreateSettings: BeelineRecreateSettings): Either[StatusCode, (SuggestRequest, Option[Long])] = {

    val suggestion = suggestionsSource.byId(suggestionId).map {
      case s if s.userId.contains(authUserId) => Right((
        SuggestRequest(
          startLat = s.startLngLat._2,
          startLng = s.startLngLat._1,
          endLat = s.endLngLat._2,
          endLng = s.endLngLat._1,
          time = s.time,
          daysOfWeek = s.daysOfWeek,
          settings = recreateSettings
        ),
        s.lastTriggerMillis
      ))
      case _ => Left(StatusCodes.Unauthorized)
    }

    suggestion.getOrElse(Left(StatusCodes.NotFound))
  }

  private def fetchNextWorkingDay: Future[ZonedDateTime] = {
    case class DateEntry(date: String, summary: String)

    val publicHolidaysFut = for {
      response <- http.singleRequest(HttpRequest(
        uri=s"${authSettings.beelineServer}/publicHolidays"
      ))
      _ <- expectStatus(response, "Could not fetch public holidays")
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

  private def getGoogleMapsTimingsAndPaths(route: Route2, workingDate: LocalDate, arrivalTime: Int)
  : Future[(List[Int], List[Option[String]])] = {
    // obtain a list of stops
    // assume a dwell time of 1 minute at each stop
    case class Intermediate(arrivalTime: Int,
                            mapsQueryResult: Option[MapsQueryResult] = None)

    val arrivalTimesAndPaths = route.stops.sliding(2).foldRight( Future(Intermediate(arrivalTime) :: Nil) )({
      case (List(from, to), prev) =>
        for {
          acc <- prev
          Intermediate(lastArrivalTime, _) :: _ = acc
          mapsQueryResult <-
            mapsAPIQuery.getP2PGoogleMapsTravelTime(
              from.coordinates, to.coordinates,
              workingDay = workingDate,
              // Previously we used arrival_time. However, this is not possible
              // if we want to let Google estimate the duration in traffic.
              // So we provide Google the estimated "departure_time" = expected arrival time - previous compute time
              departureTime = lastArrivalTime - BuiltIn.distanceMatrix(from.index)(to.index).toInt,
              googleMapsApiKey = authSettings.googleMapsApiKey)
        } yield {
          // 1-minute dwell time imputed...
          val nextArrivalTime = lastArrivalTime - IMPUTED_DWELL_TIME - mapsQueryResult.travelTime
          Intermediate(nextArrivalTime, Some(mapsQueryResult)) :: acc
        }
    })

    arrivalTimesAndPaths
      .map { immList =>
        val travelTimes = immList.map(_.arrivalTime)
        val paths = immList.map(_.mapsQueryResult.flatMap(_.encodedPath))

        (travelTimes, paths)
      }
      .recoverWith { case _ =>
        Future.failed { exc.FailedToEstimateTravelTime() }
      }
  }

  private def notifyServerOfError(suggestionId: Int, reason: String): Future[Int] = {
    import io.circe.syntax._

    suggestionsSource.insertSuggestedRoute(
      suggestionId,
      Json.obj(
        "status" -> "Failure".asJson,
        "reason" -> reason.asJson
      )
    )
  }

  private def pushToServer(suggestionId: Int,
                           googleMapsTimings: List[Int],
                           paths: List[Option[String]],
                           route: Route2) = {

    val stopToStopIdFut = syncBusStops(route)

    for {
      stopToStopId <- stopToStopIdFut
      route <- {
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
            "numAlight" -> 0.asJson,
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

        val stops = Json.arr((pickups ++ dropoffs, googleMapsTimings, paths).zipped.map {
          case (stopJson, time, path) =>
            stopJson.mapObject(_
              .add("time", time.asJson)
              .add("pathToNext", path.asJson)
            )
        } : _*)

        Future.successful(
          Json.obj(
            "stops" -> stops,
            "status" -> "Success".asJson
          )
        )
      }
      response <- suggestionsSource.insertSuggestedRoute(suggestionId, route)
    } yield {
      response
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
      import sg.beeline.util.squaredDistance
      val xy = Projections.toSVY((busStopDB.coordinates.coordinates(0), busStopDB.coordinates.coordinates(1)))

      { stop: BusStop => squaredDistance(stop.xy, xy) }
    }

    {
      for {
        allBusStopsResponse <- http.singleRequest(HttpRequest(
          uri = s"${authSettings.beelineServer}/stops"
        ))
        _ <- expectStatus(allBusStopsResponse, s"Could not verify suggestion")
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
    }.recoverWith { case _ => Future.failed(exc.FailedToGenerateStops()) }
  }
}

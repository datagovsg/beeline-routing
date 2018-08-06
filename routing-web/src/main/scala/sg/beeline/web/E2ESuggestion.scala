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
import sg.beeline.problem.{Dropoff, Pickup, Route, Suggestion}
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
                  routes <- (routingActor ? suggestRequest).mapTo[List[Route]]
                  _ <- {
                    if (routes.isEmpty) {
                      Future.unit // FIXME: push to server the empty result
                    } else {
                      val bestRoute = routes.maxBy(_.requestsInfo.size)

                      for {
                        googleMapsTimings <- {
                          getGoogleMapsTimings(bestRoute, suggestRequest.time.toInt)
                        }
                        result <- pushToServer(
                          suggestionId,
                          authorization,
                          googleMapsTimings,
                          bestRoute
                        )
                      } yield result
                    }
                  }
                } yield ()

                fut onComplete {
                  case Failure(e) => e.printStackTrace(System.err); println(fut)
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
                                 userId: Int,
                                 recreateSettings: BeelineRecreateSettings): Future[SuggestRequest] = {
    for {
      resp <- http.singleRequest(HttpRequest(
        uri = s"${authSettings.beelineServer}/suggestions/${suggestionId}"
      ).withHeaders(List(
        new RawHeader("Authorization", authorization)
      )))
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
      } yield
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

      suggestionEither.right.get
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

  private def getP2PGoogleMapsTravelTime(from: Point, to: Point, arrivalTime: Int): Future[Int] = {
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
            "arrival_time" -> arrivalTime.toString,
            "key" -> authSettings.googleMapsApiKey
          ))
      ))
      s <- Unmarshal(response._3).to[Json]
    } yield {
      val qresult = s.as[QResult].right.get
      qresult.routes.head.legs.map(_.duration.value).sum
    }
  }

  private def getGoogleMapsTimings(route: Route, arrivalTime: Int): Future[List[Int]] = {
    // obtain a list of stops
    // assume a dwell time of 1 minute at each stop
    val stops = route.stops.sliding(2).foldRight( Future((List[Int](), arrivalTime)) )({
      case (List(from, to), prev) =>
        for {
          (travelTimes, nextArrivalTime) <- prev
          travelTime <- getP2PGoogleMapsTravelTime(from.coordinates, to.coordinates, nextArrivalTime)
        } yield (travelTime :: travelTimes, nextArrivalTime - 60000 - travelTime) // 1-minute dwell time imputed...
    })

    stops.map { _._1 }
  }

  private def pushToServer(
                            suggestionId: Int,
                            authorization: String,
                            googleMapsTimings: List[Int],
                            route: Route): Future[Unit] = {

    val submission: Json = Json.arr((route.stopActivities, googleMapsTimings).zipped.map {
      case ((stop, activities), time) =>
        Json.obj(
          "lat" -> Json.fromDoubleOrNull(stop.coordinates._2),
          "lng" -> Json.fromDoubleOrNull(stop.coordinates._1),
          "time" -> Json.fromInt(time),
          "busStopIndex" -> Json.fromInt(stop.index),
          "description" -> Json.fromString(stop.description),
          "numBoard" -> Json.fromInt(activities.count(_.isInstanceOf[Pickup])),
          "numAlight" -> Json.fromInt(activities.count(_.isInstanceOf[Dropoff]))
        )
    } : _*)

    for {
      entity <- Marshal(submission).to[RequestEntity]
      response <- http.singleRequest(
        HttpRequest(
          uri=Uri(s"${authSettings.beelineServer}/suggestions/${suggestionId}/route")
        )
          .withHeaders(new RawHeader("Authorization", authorization))
          .withEntity(entity)
      )
    } yield response match {
      case HttpResponse(StatusCodes.OK, _, _, _) => Success(())
      case r => Failure(new RuntimeException(s"Posting of suggested route returned ${r.status.value}"))
    }
  }
}

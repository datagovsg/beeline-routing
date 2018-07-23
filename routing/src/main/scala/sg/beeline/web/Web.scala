package sg.beeline.web

import java.util.{NoSuchElementException, UUID}

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.{Directives, ExceptionHandler}
import sg.beeline.io.DataSource
import sg.beeline.jobs.{JobQueue, RouteActor}
import sg.beeline.problem._
import sg.beeline.ruinrecreate.BeelineRecreateSettings
import sg.beeline.util.{Geo, Util, kdtreeQuery}
import akka.http.scaladsl.unmarshalling.Unmarshaller
import ch.megard.akka.http.cors.scaladsl.CorsDirectives.cors

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success, Try}

case class SuggestRequest(startLat: Double,
                          startLng: Double,
                          endLat: Double,
                          endLng: Double,
                          time: Double,
                          settings: BeelineRecreateSettings)
case class LatLng(lat : Double, lng : Double)

trait JsonSupport extends JsonMarshallers {
  import _root_.io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}

  implicit val latLngEncoder = deriveEncoder[LatLng]
  implicit val busStopEncoder = BusStopEncoder
  implicit val routeFormat = RouteJsonEncoder
  implicit val suggestionFormat = SuggestionJsonEncoder
  implicit val requestFormat = RequestJsonEncoder
  implicit val beelineRecreateSettingsDecoder = deriveDecoder[BeelineRecreateSettings]

  implicit val beelineRecreateSettingsUnmarshaller: Unmarshaller[String, BeelineRecreateSettings]
  = stringToJsonUnmarshaller
    .map(s => s.as[BeelineRecreateSettings] match {
      case Right(t) => t
      case Left(e) => throw e
    })
}

// this trait defines our service behavior independently from the service actor
class IntelligentRoutingService(dataSource: DataSource,
                                suggestionsSource: Seq[Suggestion])
  extends Directives with JsonSupport {
  import akka.actor._
  import _root_.io.circe.syntax._

  import ExecutionContext.Implicits.global
  implicit val timeout = new akka.util.Timeout(300e3.toLong, java.util.concurrent.TimeUnit.MILLISECONDS)
  implicit val system = ActorSystem()
  val routingActor = system.actorOf(Props({
    new RouteActor(dataSource, _ => suggestionsSource)
  }), "intelligent-routing")
  val jobQueue = new JobQueue[SuggestRequest, List[Route]](
    routingActor, 10 minutes,5 minutes)

  val myRoute = cors() {
    path("bus_stops") {
      get {
        complete(dataSource.busStops.asJson)
      }
    } ~
    path("bus_stops" / Remaining) { remaining =>
      get {
        val requestedSet = remaining.split("/")
          .filter(_ != "")
          .map(s => s.toInt)
          .map(dataSource.busStopsByIndex)
        val finalSet: Seq[BusStop] =
          if (requestedSet.isEmpty) dataSource.busStops // all bus stops
          else requestedSet

        complete(finalSet.asJson)
      }
    } ~
    path("paths" / Remaining) { remaining =>
      get {
        val busStops = dataSource.busStopsByIndex
        val indices = remaining.split("/").filter(_ != "").map(s => s.toInt)

        val polyline = indices.sliding(2).map({
          case Array(aIndex, bIndex) =>
            val busStopA = busStops(aIndex)
            val busStopB = busStops(bIndex)

            Geo.travelPath(
              busStopA.coordinates, busStopA.heading,
              busStopB.coordinates, busStopB.heading
            ).toList
        }).toList

        complete(
          polyline.map(_.map({case (x,y) => LatLng(y,x)})).asJson
        )
      }
    } ~
    path("travel_times" / Remaining) { remaining =>
      get {
        val busStops = dataSource.busStops
        val indices = remaining.split("/").filter(_ != "").map(s => s.toInt)

        val travelTimes: Seq[Double] = indices.sliding(2).map({
          case Array(aIndex, bIndex) =>
            dataSource.distanceFunction(
              dataSource.busStopsByIndex(aIndex),
              dataSource.busStopsByIndex(bIndex)
            )
        }).toArray

        complete(travelTimes.asJson)
      }
    } ~
    /**
      * returns the requests that are served by this route
      */
    path("path_requests" / Remaining) { remaining =>
      get {
        parameters(
          'maxDistance.as[Double]
        ) { maxDistance =>
          def withinReach(p: Util.Point, q: Util.Point) =
            kdtreeQuery.squaredDistance(p, q) <= maxDistance * maxDistance

          /**
            * A list of stops serve a suggestion if
            * there is a stop A that serves the pickup point, and a stop B that
            * serves the dropoff point, and A comes before B in the list of stops
            *
            * i.e. minimum index of stops that serve the pickup point is
            * less than the maximum index that serve the dropoff point
            */
          def pathServesSuggestion(busStops : IndexedSeq[BusStop], suggestion: Suggestion) = {
            val minPickupStop =
              busStops.indices
                .filter(i => withinReach(busStops(i).xy, suggestion.start))

            val maxDropoffStop =
              busStops.indices
                .filter(i => withinReach(busStops(i).xy, suggestion.end))

            minPickupStop.nonEmpty &&
              maxDropoffStop.nonEmpty &&
              minPickupStop.min < maxDropoffStop.max
          }

          val busStops = remaining.split("/").filter(_ != "")
            .map(s => dataSource.busStopsByIndex(s.toInt))

          complete({
            suggestionsSource
            .filter(suggestion => pathServesSuggestion(busStops, suggestion))
          }.asJson)
        }
      }
    } ~
    path("routing" / "begin") {
      get {
        parameters(
          'startLat.as[Double],
          'startLng.as[Double],
          'endLat.as[Double],
          'endLng.as[Double],
          'time.as[Double],
          'settings.as[BeelineRecreateSettings]
        ).as(SuggestRequest) { suggestRequest =>
          complete {
            jobQueue.enqueueJob(suggestRequest).toString
          }
        }
      }
    } ~
    path("routing" / "poll") {
      get {
        parameters(
          'uuid.as[String]
        ) { uuidStr =>
          implicit val timeout = new akka.util.Timeout(300e3.toLong, java.util.concurrent.TimeUnit.MILLISECONDS)

          val uuidTry = Try { UUID.fromString(uuidStr) }

          import sg.beeline.jobs.JobResultActor._

          uuidTry match {
            case Failure(_) => complete(StatusCodes.BadRequest)
            case Success(uuid) =>
              onSuccess(jobQueue.getStatus(uuid)) {
                case JobQueued() =>
                  complete((StatusCodes.Accepted, "The job is still queued"))
                case JobRunning() =>
                  complete((StatusCodes.Accepted, "The job is still running"))
                case JobNotFound() =>
                  complete(StatusCodes.NotFound)
                case JobFailed(exc) =>
                  complete((
                    StatusCodes.InternalServerError,
                    s"The job errored with: ${exc.getMessage}"))
                case JobSucceeded(res) =>
                  complete(res.asJson)
              }
          }
        }
      }
    }
  }
}

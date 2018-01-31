package sg.beeline.web

import java.util.{NoSuchElementException, UUID}

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.marshalling.GenericMarshallers._
import akka.http.scaladsl.marshalling.PredefinedToEntityMarshallers._
import akka.http.scaladsl.marshalling.PredefinedToResponseMarshallers._
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.{Directives, ExceptionHandler}
import akka.pattern.ask
import sg.beeline.JobQueueActor
import sg.beeline.JobQueueActor.{InitRequest, PollResult, ResultPendingException}
import sg.beeline.io.Import
import sg.beeline.jobs.RouteActor
import sg.beeline.problem.{Dropoff, Pickup, Route, BusStop, Suggestion, Request}
import sg.beeline.ruinrecreate.BeelineRecreateSettings
import sg.beeline.util.{Geo, Util, kdtreeQuery}
import spray.json._

import scala.concurrent.ExecutionContext
import scala.util.Try

case class Stop(busStop : BusStop, numBoard : Int, numAlight: Int) {}
case class RouteWithPath(route: Route)

object SuggestionJsonFormat extends RootJsonFormat[Suggestion] {
  def write(suggestion: Suggestion) =
    JsObject(
      "id" -> JsNumber(suggestion.id),
      "start" -> RouteJsonFormat.latLng(Util.toWGS(suggestion.start)),
      "end" -> RouteJsonFormat.latLng(Util.toWGS(suggestion.end)),
      "time" -> JsNumber(suggestion.actualTime)
    )
  def read(value : JsValue) = throw new UnsupportedOperationException()
}

object RequestJsonFormat extends RootJsonFormat[Request] {
  def write(request: Request) =
    JsObject(
      "start" -> RouteJsonFormat.latLng(Util.toWGS(request.start)),
      "end" -> RouteJsonFormat.latLng(Util.toWGS(request.end)),
      "time" -> JsNumber(request.actualTime)
    )
  def read(value : JsValue) = throw new UnsupportedOperationException()
}

object RouteJsonFormat extends RootJsonFormat[Route] {
  def latLng(d: (Double, Double)) = JsObject(
    "lat" -> JsNumber(d._2),
    "lng" -> JsNumber(d._1)
  )

  def write(route: Route) : JsValue = {
    val positions = route.activitiesWithTimes.flatMap({
      case (Pickup(r, l), minTime, maxTime) => Some((Stop(l, 1, 0), minTime, maxTime))
      case (Dropoff(r, l), minTime, maxTime) => Some((Stop(l, 0, 1), minTime, maxTime))
      case _ => None
    }).foldRight(
        List[(Stop, Double, Double)]()
      ) { // Remove consecutive runs
        case ((Stop(loc, a, b), minTime, maxTime), Nil) =>
          (Stop(loc, a, b), minTime, maxTime) :: Nil
        case ((Stop(loc1, a1, b1), minTime1, maxTime1), (Stop(loc2, a2, b2), minTime2, maxTime2)::tail) =>
          if (loc1 == loc2)
            (Stop(loc1, a1 + a2, b1 + b2), minTime1, maxTime1) ::tail
          else
            (Stop(loc1, a1, b1), minTime1, maxTime1) :: (Stop(loc2, a2, b2), minTime2, maxTime2) :: tail
      }

    val positionsJson = positions.map({ case (Stop(bs, board, alight), minTime, maxTime) =>
      JsObject(
        latLng(bs.coordinates).fields ++
        List(
          ("description" -> JsString(bs.description)),
          ("numBoard" -> JsNumber(board)),
          ("numAlight" -> JsNumber(alight)),
          ("index" -> JsNumber(bs.index)),
          ("minTime" -> JsNumber(minTime)),
          ("maxTime" -> JsNumber(maxTime))
        )
      )
    }).toList

    val requestsJson = route.activities
      .flatMap({ case Pickup(request, loc) => Some(request) case _ => None})
      .map(request => RequestJsonFormat.write(request))
      .toList

    JsObject(
      "stops" -> JsArray(positionsJson),
      "requests" -> JsArray(requestsJson)
    )
  }

  def read(value : JsValue) = throw new UnsupportedOperationException()
}

case class CircularRegionRequest(lat : Double, lng : Double, radius : Double) {}
case class RoutingRequest(times: List[Double], regions : List[CircularRegionRequest]) {}
case class PathRequest(indices: List[Int]) {}
case class SuggestRequest(startLat: Double,
                          startLng: Double,
                          endLat: Double,
                          endLng: Double,
                          time: Double,
                          settings: BeelineRecreateSettings)
case class PathRequestsRequest(maxDistance: Double)
case class LatLng(lat : Double, lng : Double)

trait JsonSupport extends SprayJsonSupport with DefaultJsonProtocol {
  implicit val latLngFormat = jsonFormat2(LatLng)
  implicit val busStopFormat : JsonFormat[BusStop] = jsonFormat[
    (Double, Double), Double, String, String, Int, BusStop
    ](
    BusStop,
    "coordinates", "heading", "description", "roadName", "index"
  )
  implicit val routeFormat = RouteJsonFormat
  implicit val suggestionFormat = SuggestionJsonFormat
  implicit val requestFormat = RequestJsonFormat
  implicit val beelineRecreateSettingsFormat = jsonFormat6(BeelineRecreateSettings.apply)
}

// this trait defines our service behavior independently from the service actor
object IntelligentRoutingService extends Directives with JsonSupport {
  import akka.actor._

  import ExecutionContext.Implicits.global
  implicit val timeout = new akka.util.Timeout(300e3.toLong, java.util.concurrent.TimeUnit.MILLISECONDS)
  implicit val system = ActorSystem()
  val routingActor = system.actorOf(Props[RouteActor], "intelligent-routing")
  val jobQueueActor = system.actorOf(Props(new JobQueueActor(routingActor)), "job-queue")

  val myRoute =
    path("bus_stops") {
      get {
        complete(Import.getBusStopsOnly)
      }
    } ~
    path("bus_stops" / Remaining) { remaining =>
      get {
        val requestedSet = remaining.split("/").filter(_ != "").map(s => s.toInt)

        complete(
          if (requestedSet.isEmpty)
            Import.getBusStopsOnly
          else
            Import.getBusStopsOnly.filter(requestedSet contains _.index)
        )
      }
    } ~
    path("paths" / Remaining) { remaining =>
      get {
        val busStops = Import.getBusStopsOnly
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
          polyline.map(_.map({case (x,y) => LatLng(y,x)}))
        )
      }
    } ~
    path("travel_times" / Remaining) { remaining =>
      get {
        val busStops = Import.getBusStopsOnly
        val indices = remaining.split("/").filter(_ != "").map(s => s.toInt)

        val travelTimes: Seq[Double] = indices.sliding(2).map({
          case Array(aIndex, bIndex) =>
            Import.distanceMatrix(aIndex)(bIndex)
        }).toArray

        complete(travelTimes)
      }
    } ~
    /**
      * returns the requests that are served by this route
      */
    path("path_requests" / Remaining) { remaining =>
      get {
        parameters(
          'maxDistance.as[Double]
        ).as(PathRequestsRequest) { r =>
          def withinReach(p: Util.Point, q: Util.Point) =
            kdtreeQuery.squaredDistance(p, q) <= r.maxDistance * r.maxDistance

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
              busStops.indices.filter(i => withinReach(busStops(i).xy, suggestion.start))
                .foldLeft(Int.MaxValue)(_ min _)
            val maxDropoffStop =
              busStops.indices.filter(i => withinReach(busStops(i).xy, suggestion.end))
                .foldLeft(Int.MinValue)(_ max _)

            minPickupStop < maxDropoffStop
          }

          complete {
            val allBusStops = Import.getBusStopsOnly
            val busStops = remaining.split("/").filter(_ != "")
              .map(s => allBusStops(s.toInt))
            val suggestions = Import.getLiveRequests()

            suggestions
            .filter(suggestion => pathServesSuggestion(busStops, suggestion))
          }
        }
      }
    } ~
    path("routes" / "propose") {
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
            implicit val timeout = new akka.util.Timeout(300e3.toLong, java.util.concurrent.TimeUnit.MILLISECONDS)

            (routingActor ? suggestRequest)
                .map(_.asInstanceOf[Try[List[Route]]])
          }
        } ~
        {
          complete(StatusCodes.BadRequest)
        }
      }
    } ~
    path("routing" / "begin") {
      get {
        implicit val timeout = new akka.util.Timeout(300e3.toLong, java.util.concurrent.TimeUnit.MILLISECONDS)

        parameters(
          'startLat.as[Double],
          'startLng.as[Double],
          'endLat.as[Double],
          'endLng.as[Double],
          'time.as[Double],
          'settings.as[BeelineRecreateSettings]
        ).as(SuggestRequest) { suggestRequest =>
          complete {
            (jobQueueActor ? InitRequest(suggestRequest))
              .map({
                case uuid: String => uuid
              })
          }
        }
      }
    } ~
    path("routing" / "poll") {
      get {
        parameters(
          'uuid.as[String]
        ) { uuid =>
          implicit val timeout = new akka.util.Timeout(300e3.toLong, java.util.concurrent.TimeUnit.MILLISECONDS)
          handleExceptions(ExceptionHandler {
            case e: NoSuchElementException =>
              complete((StatusCodes.BadRequest, "The job was not found"))
            case e: ResultPendingException =>
              complete((StatusCodes.Accepted, "The job is still pending"))
          }) {
            complete {
              (jobQueueActor ? PollResult(UUID.fromString(uuid)))
                .map(_.asInstanceOf[Try[Try[List[Route]]]])
            }
          }
        }
      }
    }
}

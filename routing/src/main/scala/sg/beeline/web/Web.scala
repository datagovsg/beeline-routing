package sg.beeline.web

import java.util.{NoSuchElementException, UUID}

import akka.http.scaladsl.model.{HttpCharsets, HttpEntity, MediaType, StatusCodes}
import akka.http.scaladsl.server.{Directives, ExceptionHandler}
import akka.pattern.ask
import sg.beeline.JobQueueActor
import sg.beeline.JobQueueActor.{InitRequest, PollResult, ResultPendingException}
import sg.beeline.io.Import
import sg.beeline.jobs.RouteActor
import sg.beeline.problem._
import sg.beeline.ruinrecreate.BeelineRecreateSettings
import sg.beeline.util.{Geo, Util, kdtreeQuery}
import _root_.io.circe._
import akka.http.scaladsl.marshalling.PredefinedToEntityMarshallers
import akka.http.scaladsl.server.directives.ParameterDirectives.ParamMagnet
import akka.http.scaladsl.unmarshalling.{PredefinedFromEntityUnmarshallers, PredefinedFromStringUnmarshallers, Unmarshaller}

import scala.concurrent.ExecutionContext
import scala.util.Try

case class Stop(busStop : BusStop, numBoard : Int, numAlight: Int) {}
case class RouteWithPath(route: Route)

object SuggestionJsonEncoder extends Encoder[Suggestion] {
  override def apply(suggestion: Suggestion): Json =
    Json.obj(
      "id" -> Json.fromInt(suggestion.id),
      "start" -> Json.fromFields(RouteJsonEncoder.latLng(Util.toWGS(suggestion.start))),
      "end" -> Json.fromFields(RouteJsonEncoder.latLng(Util.toWGS(suggestion.end))),
      "time" -> Json.fromDouble(suggestion.actualTime).get
    )
}

object RequestJsonEncoder extends Encoder[Request] {
  override def apply(request: Request) =
    Json.obj(
      "start" -> Json.fromFields(RouteJsonEncoder.latLng(Util.toWGS(request.start))),
      "end" -> Json.fromFields(RouteJsonEncoder.latLng(Util.toWGS(request.end))),
      "time" -> Json.fromDouble(request.actualTime).get
    )
}

object RouteJsonEncoder extends Encoder[Route] {
  def latLng(d: (Double, Double)) = List(
    "lat" -> Json.fromDouble(d._2).get,
    "lng" -> Json.fromDouble(d._1).get
  )

  override def apply(route: Route) : Json = {
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
      Json.fromFields(
        latLng(bs.coordinates) ++
        List(
          "description" -> Json.fromString(bs.description),
          "numBoard" -> Json.fromInt(board),
          "numAlight" -> Json.fromInt(alight),
          "index" -> Json.fromInt(bs.index),
          "minTime" -> Json.fromDouble(minTime).get,
          "maxTime" -> Json.fromDouble(maxTime).get
        )
      )
    })

    val requestsJson = route.activities
      .flatMap({ case Pickup(request, loc) => Some(request) case _ => None})
      .map(request => RequestJsonEncoder(request))
      .toList

    Json.obj(
      "stops" -> Json.arr(positionsJson:_*),
      "requests" -> Json.arr(requestsJson:_*)
    )
  }
}

object BusStopEncoder extends Encoder[BusStop] {
  override def apply(a: BusStop): Json = a match {
    case BusStop((x, y), heading, description, roadName, index) =>
      Json.obj(
        "coordinates" -> Json.arr(Json.fromDoubleOrNull(x), Json.fromDoubleOrNull(y)),
        "heading" -> Json.fromDoubleOrNull(heading),
        "description" -> Json.fromString(description),
        "roadName" -> Json.fromString(roadName),
        "index" -> Json.fromInt(index)
      )
  }
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

trait JsonSupport extends PredefinedToEntityMarshallers {
  import _root_.io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}

  implicit val latLngEncoder = deriveEncoder[LatLng]
  implicit val busStopEncoder = BusStopEncoder
  implicit val routeFormat = RouteJsonEncoder
  implicit val suggestionFormat = SuggestionJsonEncoder
  implicit val requestFormat = RequestJsonEncoder
  implicit val beelineRecreateSettingsDecoder = deriveDecoder[BeelineRecreateSettings]

  implicit val jsonMarshaller = stringMarshaller(
    MediaType.applicationWithFixedCharset(
      "json",
      HttpCharsets.`UTF-8`)
  )
    .compose(
      (json: Json) => _root_.io.circe.Printer.noSpaces.pretty(json)
    )

  /* Needed to unmarshall JSON in Query Params */
  implicit val jsonUnmarshaller = Unmarshaller.strict[String, Json](s =>
    _root_.io.circe.parser.parse(s) match {
      case Right(t) => t
      case Left(e) => throw e
    })
  implicit val beelineRecreateSettingsUnmarshaller: Unmarshaller[String, BeelineRecreateSettings]
  = jsonUnmarshaller
    .map(s => s.as[BeelineRecreateSettings] match {
      case Right(t) => t
      case Left(e) => throw e
    })
}

// this trait defines our service behavior independently from the service actor
object IntelligentRoutingService extends Directives with JsonSupport {
  import akka.actor._
  import _root_.io.circe.syntax._

  import ExecutionContext.Implicits.global
  implicit val timeout = new akka.util.Timeout(300e3.toLong, java.util.concurrent.TimeUnit.MILLISECONDS)
  implicit val system = ActorSystem()
  val routingActor = system.actorOf(Props[RouteActor], "intelligent-routing")
  val jobQueueActor = system.actorOf(Props(new JobQueueActor(routingActor)), "job-queue")

  val myRoute =
    path("bus_stops") {
      get {
        complete(Import.getBusStopsOnly.asJson)
      }
    } ~
    path("bus_stops" / Remaining) { remaining =>
      get {
        val requestedSet = remaining.split("/").filter(_ != "").map(s => s.toInt)

        complete({
          if (requestedSet.isEmpty)
            Import.getBusStopsOnly
          else
            Import.getBusStopsOnly.filter(requestedSet contains _.index)
        }.asJson)
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
          polyline.map(_.map({case (x,y) => LatLng(y,x)})).asJson
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

          complete({
            val allBusStops = Import.getBusStopsOnly
            val busStops = remaining.split("/").filter(_ != "")
              .map(s => allBusStops(s.toInt))
            val suggestions = Import.getLiveRequests()

            suggestions
            .filter(suggestion => pathServesSuggestion(busStops, suggestion))
          }.asJson)
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
            implicit val timeout = new akka.util.Timeout(300e3.toLong,
              java.util.concurrent.TimeUnit.MILLISECONDS)

            (routingActor ? suggestRequest)
              .mapTo[Try[List[Route]]]
              .map(_.map(_.asJson))
          }
        } ~
        {
          complete(StatusCodes.BadRequest)
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
            implicit val timeout = new akka.util.Timeout(300e3.toLong, java.util.concurrent.TimeUnit.MILLISECONDS)

            (jobQueueActor ? InitRequest(suggestRequest))
              .mapTo[String]
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
                .mapTo[Try[Try[List[Route]]]]
                .map(_.flatten.map(_.asJson))
            }
          }
        }
      }
    }
}

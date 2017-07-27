package sg.beeline

import akka.actor.{Props, ActorRef, Actor}
import akka.http.scaladsl.model
import akka.pattern.ask
import org.json4s.{FieldSerializer, CustomSerializer, DefaultFormats}
import sg.beeline.ui._
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.marshalling.GenericMarshallers._
import akka.http.scaladsl.marshalling.PredefinedToEntityMarshallers._
import akka.http.scaladsl.marshalling.PredefinedToResponseMarshallers._
import akka.http.scaladsl.server.Directives
import akka.http.scaladsl.model.StatusCodes
import spray.json._
import scala.util.{Try, Success, Failure}
import scala.concurrent.Future

import scala.concurrent.ExecutionContext

case class Stop(busStop : BusStop, numBoard : Int, numAlight: Int) {}
case class RouteWithPath(route: Route)

object RouteJsonFormat extends RootJsonFormat[Route] {
  def write(route: Route) = {
    val positions = route.activities.flatMap({
      case Pickup(r, l) => Some(Stop(l, 1, 0))
      case Dropoff(r, l) => Some(Stop(l, 0, 1))
      case _ => None
    }).foldRight(
        List[Stop]()
      ) { // Remove consecutive runs
        case (Stop(loc, a, b), Nil) => Stop(loc, a, b) :: Nil
        case (Stop(loc1, a1, b1), Stop(loc2, a2, b2)::tail) =>
          if (loc1 == loc2)
            Stop(loc1, a1 + a2, b1 + b2) ::tail
          else
            Stop(loc1, a1, b1) :: Stop(loc2, a2, b2) :: tail
      }

    def latLng(d: (Double, Double)) = JsObject(
      "lat" -> JsNumber(d._2),
      "lng" -> JsNumber(d._1)
    )

    val positionsJson = positions.map({ case Stop(bs, board, alight) =>
      JsObject(
        latLng(bs.coordinates).fields ++
        List(
          ("description" -> JsString(bs.description)),
          ("numBoard" -> JsNumber(board)),
          ("numAlight" -> JsNumber(alight)),
          ("index" -> JsNumber(bs.index))
        )
      )
    }).toList

    val requestsJson = route.activities
      .flatMap({ case Pickup(request, loc) => Some(request) case _ => None})
      .map(request => JsObject(
        "start" -> latLng(Util.toWGS(request.start)),
        "end" -> latLng(Util.toWGS(request.end)),
        "time" -> JsNumber(request.actualTime)
      ))
      .toList

    JsObject(
      "stops" -> JsArray(positionsJson),
      "requests" -> JsArray(requestsJson)
    )
  }

  def read(value : JsValue) = throw new UnsupportedOperationException()
}

case class CircularRegionRequest(val lat : Double, val lng : Double, val radius : Double) {}
case class RoutingRequest(val times: List[Double], val regions : List[CircularRegionRequest]) {}
case class PathRequest(val indices: List[Int]) {}
case class SuggestRequest(startLat: Double,
                          startLng: Double,
                          endLat: Double,
                          endLng: Double,
                          time: Double,
                          settings: BeelineRecreateSettings)
case class LatLng(val lat : Double, val lng : Double)

trait JsonSupport extends SprayJsonSupport with DefaultJsonProtocol {
  implicit val latLngFormat = jsonFormat2(LatLng)
  implicit val busStopFormat : JsonFormat[BusStop] = jsonFormat[
    (Double, Double), Double, String, String, Int, BusStop
    ](
    sg.beeline.BusStop,
    "coordinates", "heading", "description", "roadName", "index"
  )
  implicit val routeFormat = RouteJsonFormat
  implicit val beelineRecreateSettingsFormat = jsonFormat6(sg.beeline.BeelineRecreateSettings.apply)
}

// this trait defines our service behavior independently from the service actor
object IntelligentRoutingService extends Directives with JsonSupport {
  import ExecutionContext.Implicits.global
  import akka.actor._

  implicit val system = ActorSystem()
  val routingActor = system.actorOf(Props[RouteActor], "intelligent-routing")

  val myRoute =
    path("bus_stops") {
      get {
        complete(Import.getBusStops)
      }
    } ~
    path("paths" / Remaining) { remaining =>
      get {
        val busStops = Import.getBusStops
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
    }
}

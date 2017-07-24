package sg.beeline

import akka.actor.{Props, ActorRef, Actor}
import akka.pattern.ask
import org.json4s.{FieldSerializer, CustomSerializer, DefaultFormats}
import org.json4s.JsonAST._
import sg.beeline.ui._
import spray.httpx.{Json4sSupport, SprayJsonSupport}
import spray.routing._
import spray.http._
import MediaTypes._
import org.json4s.JsonDSL._

import scala.concurrent.ExecutionContext

case class Stop(busStop : BusStop, numBoard : Int, numAlight: Int) {}
case class RouteWithPath(route: Route)

object RouteSerializer extends CustomSerializer[RouteWithPath](format => {
  implicit val json4sFormats = DefaultFormats

  (
    {
      null
    },
    {
      case route : Route => {
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

        def latLng(d : (Double, Double)) =
          ("lat" -> d._2) ~ ("lng" -> d._1)

        val positionsJson = positions.map({case Stop(bs, board, alight) =>
          latLng(bs.coordinates) ~
            ("description" -> bs.description) ~
            ("numBoard" -> board) ~
            ("numAlight" -> alight) ~
            ("index" -> bs.index)
        })

        ("stops" -> positionsJson) ~
//          ("path" -> routePath.map({case (x,y) => ("lat" -> y) ~ ("lng" -> x)}).toList) ~
          ("requests" -> route.activities.flatMap({ case Pickup(request, loc) => Some(request) case _ => None})
                .map(request => ("start" -> latLng(Util.toWGS(request.start))) ~
                  ("end" -> latLng(Util.toWGS(request.end))) ~
                  ("time" -> request.actualTime) ~
                  ("weight" -> request.weight)))

      }
    }
  )
})

object AllBusStops {
  implicit val formats = DefaultFormats
  val busStops = Import.getBusStops
  val json =
    ("busStops" ->
      busStops.map { bs =>
        ("description" -> bs.description) ~
          ("road" -> bs.roadName) ~
          ("index" -> bs.index) ~
          ("coordinates" -> ("type" -> "Point") ~ ("coordinates" -> List(bs.coordinates._1, bs.coordinates._2)))
      })
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

// this trait defines our service behavior independently from the service actor
class IntelligentRoutingService extends HttpService with Actor with Json4sSupport {
  import ExecutionContext.Implicits.global

  implicit val json4sFormats = DefaultFormats + new FieldSerializer() + RouteSerializer

  lazy val routingActor = context.actorOf(Props[RouteActor], "routing-actor")

  // the HttpService trait defines only one abstract member, which
  // connects the services environment to the enclosing actor or test
  def actorRefFactory = context
  def receive = runRoute(myRoute)

  val myRoute =
    path("") {
      get {
        redirect("/static/index.html", StatusCodes.TemporaryRedirect)
      }
    } ~
    pathPrefix("static") {
      getFromDirectory("./static")
    } ~
    path("routing" / "start") {
      post {
        entity(as[RoutingRequest]) { request =>
          routingActor ! new StartRouting(
            request.times,
            request.regions.map(req => new CircularRegion((req.lng, req.lat), req.radius)))

          complete("")
        }
      }
    } ~
    path("routing" / "stop") {
      post {
        implicit val timeout = new akka.util.Timeout(60000, java.util.concurrent.TimeUnit.MILLISECONDS)

        // Wait for stopped...
        onSuccess(routingActor ? StopRouting) { stopped =>
          complete("")
        }
      }
    } ~
    path("routing" / "current") {
      get {
        implicit val timeout = new akka.util.Timeout(60000, java.util.concurrent.TimeUnit.MILLISECONDS)

        onSuccess(routingActor ? CurrentSolution) {
          case routes : Traversable[Route] =>
            complete(routes.par.zipWithIndex.map({
              case (r,i) => r
//                val path = r.activities.sliding(2).map({
//                  case Seq(a1, a2) => (a1.location, a2.location)
//                }).flatMap({
//                  case (Some(loc1), Some(loc2)) =>
////                    Geo.travelPath(loc1.coordinates, loc1.heading, loc2.coordinates, loc2.heading)
//                    List(loc1.coordinates, loc2.coordinates)
//                  case _ => List()
//                }).toList

//                new RouteWithPath(r)
            }).toList)
        }
      }
    } ~
    path("path") {
      post {
        implicit val timeout = new akka.util.Timeout(60000, java.util.concurrent.TimeUnit.MILLISECONDS)

        entity(as[PathRequest]) {
          case PathRequest(indices) =>
            val busStops = Import.getBusStops

            val polyline = indices.sliding(2).map({
              case List(aIndex, bIndex) =>
                val busStopA = busStops(aIndex)
                val busStopB = busStops(bIndex)
                Geo.travelPath(
                  busStopA.coordinates, busStopA.heading,
                  busStopB.coordinates, busStopB.heading
                ).toList
            }).toList

            complete(polyline.map(_.map({case (x,y) => LatLng(y,x)})))
        }
      }
    } ~
    path("bus_stops") {
      get {
        complete(AllBusStops.json)
      }
    } ~
    path("paths" / Rest) { rest =>
      get {
        val busStops = Import.getBusStops
        val indices = rest.split("/").filter(_ == "").map(s => s.toInt)

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
    }
}

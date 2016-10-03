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

class Lol(val x: Int, val y: Double) {}

case class Stop(busStop : BusStop, numBoard : Int, numAlight: Int) {}
case class RouteWithPath(route: Route, routePath: Seq[(Double, Double)])

object RouteSerializer extends CustomSerializer[RouteWithPath](format => {
  implicit val json4sFormats = DefaultFormats

  (
    {
      null
    },
    {
      case RouteWithPath(route, routePath) => {
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
            ("numAlight" -> alight)
        })

        ("stops" -> positionsJson) ~
          ("path" -> routePath.map({case (x,y) => ("lat" -> y) ~ ("lng" -> x)}).toList) ~
          ("requests" -> route.activities.flatMap({ case Pickup(request, loc) => Some(request) case _ => None})
                .map(request => (
                  ("start" -> latLng(Util.toWGS(request.start))) ~
                  ("end" -> latLng(Util.toWGS(request.end))))))

      }
    }
  )
})

class CircularRegionRequest(val lat : Double, val lng : Double, val radius : Double) {}
class RoutingRequest(val time: Double, val regions : List[CircularRegionRequest]) {}

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
          println(request.time, request.regions)
          routingActor ! new StartRouting(
            request.time,
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
            complete(routes.toList.map(r => {
              val path = r.activities.sliding(2).map({
                case Seq(a1, a2) => (a1.location, a2.location)
              }).flatMap({
                case (Some(loc1), Some(loc2)) =>
                  Geo.travelPath(loc1.coordinates, loc2.coordinates)
                case _ => List()
              }).toList

              new RouteWithPath(r, path)
            }))
        }
      }
    }
}

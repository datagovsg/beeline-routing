package sg.beeline

import akka.actor.Actor
import org.json4s.{FieldSerializer, CustomSerializer, DefaultFormats}
import org.json4s.JsonAST._
import spray.httpx.{Json4sSupport, SprayJsonSupport}
import spray.routing._
import spray.http._
import MediaTypes._
import org.json4s.JsonDSL._

import scala.collection.mutable.ArrayBuffer

// we don't implement our route structure directly in the service actor because
// we want to be able to test it independently, without having to spin up an actor
class IntelligentRoutingService extends Actor with MyService {

  // the HttpService trait defines only one abstract member, which
  // connects the services environment to the enclosing actor or test
  def actorRefFactory = context

  // this actor only runs our route, but you could add
  // other things here, like request stream processing
  // or timeout handling
  def receive = runRoute(myRoute)
}

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

        val positionsJson = positions.map({case Stop(bs, board, alight) =>
          ("lat" -> bs.coordinates._2) ~
            ("lng" -> bs.coordinates._1) ~
            ("description" -> bs.description) ~
            ("numBoard" -> board) ~
            ("numAlight" -> alight)
        })

        ("stops" -> positionsJson) ~
        ("path" -> routePath.map({case (x,y) => ("lat" -> y) ~ ("lng" -> x)}).toList)
      }
    }
  )
})

/** FIXME: use actors */
object CurrentSolution {
  var routes : Seq[Route] = List()
  var routePaths : Seq[Seq[(Double, Double)]] = List()

  def updateRoutes(routes: Seq[Route]) {
    this.routes = routes.sortBy(r => -r.activities.size)
    this.routePaths = this.routes.map(route =>
      route.activities.sliding(2).flatMap({
        case IndexedSeq(a, b) => {
          (a.location, b.location) match {
            case (Some(aloc), Some(bloc)) =>
              Geo.routeWithJitter(aloc.coordinates, bloc.coordinates) match {
                case Some(path) => {
                  val points = path.getBest.getPoints
                  val arrayBuffer = new ArrayBuffer[(Double, Double)]

                  for (i <- 0 until points.size) {
                    arrayBuffer += ((points.getLon(i), points.getLat(i)))
                  }
                  arrayBuffer.toIndexedSeq
                }
                case None => Seq()
              }
            case _ => Seq()
          }
        }
      }).toIndexedSeq
    )
  }
}

// this trait defines our service behavior independently from the service actor
trait MyService extends HttpService with Json4sSupport {
  implicit val json4sFormats = DefaultFormats + RouteSerializer //new MyCustomSerializer()

  val myRoute =
    path("") {
      get {
        redirect("/static/index.html", StatusCodes.TemporaryRedirect)
      }
    } ~
    path("currentRoutes") {
      get {
        complete((CurrentSolution.routes, CurrentSolution.routePaths).zipped.map({
          case (x,y) => RouteWithPath(x, y)
        }))
      }
    } ~
    pathPrefix("static") {
      getFromDirectory("./static")
    }
}

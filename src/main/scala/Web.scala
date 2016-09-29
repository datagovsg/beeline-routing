package sg.beeline

import akka.actor.Actor
import org.json4s.{FieldSerializer, CustomSerializer, DefaultFormats}
import org.json4s.JsonAST._
import spray.httpx.{Json4sSupport, SprayJsonSupport}
import spray.routing._
import spray.http._
import MediaTypes._
import org.json4s.JsonDSL._

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

object RouteSerializer extends CustomSerializer[Route](format => {
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

        val positionsJson = positions.map({case Stop(bs, board, alight) =>
          ("lat" -> bs.coordinates._2) ~
            ("lng" -> bs.coordinates._1) ~
            ("description" -> bs.description) ~
            ("numBoard" -> board) ~
            ("numAlight" -> alight)
        })

        ("stops" -> positionsJson)
      }
    }
  )
})

object CurrentSolution {
  var routes : Seq[Route] = List()

  def updateRoutes(routes: Seq[Route]) {
    this.routes = routes.sortBy(r => -r.activities.size)
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
        complete(CurrentSolution.routes)
      }
    } ~
    pathPrefix("static") {
      getFromDirectory("./static")
    }
}

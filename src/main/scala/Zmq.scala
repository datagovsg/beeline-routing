package sg.beeline

import java.time.Duration

import akka.actor.{Actor, ActorSystem, Props}
import akka.pattern.ask
import org.json4s.JsonAST._
import org.json4s.JsonDSL._
import org.json4s.native.JsonMethods._
import org.json4s.native.Serialization._
import org.json4s.{DefaultFormats, FieldSerializer}
import org.zeromq.{ZMQException, ZMQ}
import sg.beeline.ui._

import scala.annotation.tailrec
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.{Success, Failure}

class Exit() extends Throwable

// this trait defines our service behavior independently from the service actor
object ZmqRoutingService {
  implicit val json4sFormats = DefaultFormats + new FieldSerializer() + RouteSerializer
  import ExecutionContext.Implicits.global

  def begin(): Unit = {
    val zmqContext = ZMQ.context(1)
    val mainThread = Thread.currentThread

    def tryAgain = {
      val responder = zmqContext.socket(ZMQ.REP)
      responder.bind("tcp://*:5555")

      println("Bound to tcp 5555")
      responder
    }

    def handleRequest(request: String) : Future[String] = {
      Future[Future[String]]({
        // Convert to json
        val ast = parse(request)
        val action = (ast \ "type").extract[String]
        val handler = (ast \ "payload")

        val result = handle(action, handler).asInstanceOf[Future[AnyRef]]

        // Send reply back to client
        result.map{ case r => write(r) }
      }).flatMap(x => x)
    }

    val responder = tryAgain

    def end: Unit = {
      responder.close()
      zmqContext.close()
      zmqContext.term()
      println("Closing everything")
    }

    @tailrec
    def next : Unit = {
      if (Thread.currentThread.isInterrupted) {
        end
      }
      else {
        val continue = try {
          val request = responder.recvStr()
          val replyFuture = handleRequest(request)

          val repliedFuture = replyFuture
            .map({result =>
              responder.send(s"""{"status": "success", "payload": ${result}}""", 0)
            })
            .recover({
              case error : Exception =>
                val jsonMessage = compact(render(JString(error.getMessage)))
                responder.send(s"""{"status": "failure", "payload": ${jsonMessage}}""")
            })

          Await.result(repliedFuture, scala.concurrent.duration.Duration.Inf)
          true
        } catch {
          case e: ZMQException =>
            e.getErrorCode != ZMQ.Error.ETERM.getCode
        }

        // ???
        if (continue)
          next
        else
          end
      }
    }

    next
  }

//  lazy val routingActor = context.actorOf(Props[RouteActor], "routing-actor")
  implicit val system = ActorSystem()
  implicit val timeout = new akka.util.Timeout(30 * 60 * 1000, java.util.concurrent.TimeUnit.MILLISECONDS)
  val routingActor = system.actorOf(Props[RouteActor], "intelligent-routing")

  var lastSuggestResult : Traversable[Route] = null

  def handle(action : String, jValue : JValue) : scala.concurrent.Future[Any] = {
    action match {
      case "startRouting" =>
        val request = jValue.extract[RoutingRequest]

        routingActor ? new StartRouting(
          request.times,
          request.regions.map(req => new CircularRegion((req.lng, req.lat), req.radius)))

      case "stopRouting" =>

        routingActor ? StopRouting

      case "getCurrent" =>
        (routingActor ? CurrentSolution).map {
          case routes : Traversable[Route] =>
            routes.par.zipWithIndex.map(_._1).toList
        }
      case "getPath" =>
        val pathRequest = jValue.extract[PathRequest]

        Future({
          pathRequest match {
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
              polyline.map(_.map({case (x,y) => LatLng(y,x)}))
          }
        })

      case "suggestRoutes" =>
        val suggestRequest = jValue.extract[SuggestRequest]

        (routingActor ? suggestRequest)

      case "getBusStops" =>
        Future({
          AllBusStops.json
        })
    }
  }
}

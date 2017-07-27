package sg.beeline.ui

import java.util.UUID

import akka.actor.{ActorRef, Actor}
import sg.beeline._

import scala.concurrent.Future
import scala.util.{Try, Failure, Success}


abstract class RoutingControl
abstract class RoutingNotification

case class StartRouting(times : List[Double], regions : Seq[Region])
case class StopRouting() extends RoutingControl
case class CurrentSolution() extends RoutingControl
case class Polyline(indices : List[Int]) extends RoutingControl

case class RoutingStopped() extends RoutingNotification
case class RoutingStarted() extends RoutingNotification

class RouteActor extends Actor {
  var lastResults : Traversable[Route] = List()
  val busStops = Import.getBusStops

  def routeFromRequest(suggestRequest: SuggestRequest) = suggestRequest match {
    case SuggestRequest(sLat, sLng, eLat, eLng, time, settings) =>
      val beelineProblem = {
        println(settings.dataSource)
        val suggestions : Seq[Suggestion] = settings.dataSource match {
          case "ezlink" => Import.getEzlinkRequests
          case _ => Import.getLiveRequests
        }
        //          .map(x => new Suggestion(x.start, x.end, x.time, x.weight)) // Group them all into the same time slot
        //          .filter(x => x.time >= 8 * 3600 * 1000 && x.time <= 9 * 3600 * 1000)

        new BasicRoutingProblem(
          busStops,
          suggestions,
          startWalkingDistance = settings.startWalkingDistance,
          endWalkingDistance = settings.endWalkingDistance
        )
      }

      val beelineRecreate = new BeelineRecreate(
        beelineProblem,
        beelineProblem.requests
      )(settings)

      beelineRecreate.findRelated2(
        new Request(
          beelineProblem,
          Util.toSVY((sLng, sLat)),
          Util.toSVY((eLng, eLat)),
          time
        )
      ).toList
  }

  def receive = {
    case StartRouting(times, regions) =>
      val suggestions = sg.beeline.Import.getRequests
        .filter(x => times.contains(x.time) && regions.exists(_.contains(x.end)))
        .map(x => new Suggestion(x.start, x.end, 8 * 3600 * 1000)) // Group them all into the same time slot

//      val EZLsuggestions = sg.beeline.Import.getEzlinkRequests
//        .filter(x => times.contains(x.time) && regions.exists(_.contains(x.end)))
//        .map(x => new Suggestion(x.start, x.end, 8 * 3600 * 1000, x.weight)) // Group them all into the same time slot

      val problem = new BasicRoutingProblem(busStops, suggestions)

      val algorithm = new BasicRoutingAlgorithm(problem)

      context.become(algorithm.solve(context, (routes) => this.lastResults = routes), discardOld = false)

      sender ! RoutingStarted

    case StopRouting =>
      sender ! RoutingStopped

    case CurrentSolution =>
      sender ! lastResults

    case suggestRequest: SuggestRequest =>
      sender ! Try { routeFromRequest(suggestRequest) }

  }
}

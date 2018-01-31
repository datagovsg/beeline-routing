package sg.beeline.jobs

import akka.actor.Actor
import sg.beeline.io.Import
import sg.beeline.problem
import sg.beeline.problem._
import sg.beeline.ruinrecreate.{BasicRoutingAlgorithm, BeelineRecreate}
import sg.beeline.util.Util
import sg.beeline.web.SuggestRequest

import scala.util.Try


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
        val suggestions : Seq[Suggestion] = settings.dataSource match {
          case "ezlink" => Import.getEzlinkRequests
          case _ => Import.getLiveRequests()
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
        new BasicRequest(
          beelineProblem,
          Util.toSVY((sLng, sLat)),
          Util.toSVY((eLng, eLat)),
          time
        )
      ).toList
  }

  def receive = {
    case StartRouting(times, regions) =>
      val suggestions = Import.getLiveRequests.apply
        .view
        .filter(x => times.contains(x.time) && regions.exists(_.contains(x.end)))
      val modifiedSuggestions = suggestions
        .zipWithIndex
        .map({ case (x, i) => Suggestion(i, x.start, x.end, 8 * 3600 * 1000) }) // Group them all into the same time slot
      val suggestionsById = suggestions.map(s => (s.id, s)).toMap

      val problem = new BasicRoutingProblem(busStops, suggestions)
      val algorithm = new BasicRoutingAlgorithm(problem)

      def mapBackSuggestions(route: Route): Route =
        // Translate the activities into their original time
        new Route(
          route.routingProblem,
          route.activities.map({
            case Pickup(r, s) =>
              val modifiedSuggestion = r
                .asInstanceOf[Request.RequestFromSuggestion]
                .suggestion

              Pickup(
                new Request.RequestFromSuggestion(
                  r.routingProblem,
                  suggestionsById(modifiedSuggestion.id)
                ),
                s)
            case Dropoff(r, s) =>
              val modifiedSuggestion = r
                .asInstanceOf[Request.RequestFromSuggestion]
                .suggestion

              Dropoff(
                new Request.RequestFromSuggestion(
                  r.routingProblem,
                  suggestionsById(modifiedSuggestion.id)
                ),
                s)
            case a @ _ => a
          }),
          route.time
        )

      context.become(
        algorithm.solve(
          context,
          (routes) => this.lastResults = routes.map(mapBackSuggestions)),
        discardOld = false)

      sender ! RoutingStarted

    case StopRouting =>
      sender ! RoutingStopped

    case CurrentSolution =>
      sender ! lastResults

    case suggestRequest: SuggestRequest =>
      sender ! Try { routeFromRequest(suggestRequest) }

  }
}

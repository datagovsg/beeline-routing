package sg.beeline.jobs

import akka.actor.Actor
import sg.beeline.io.Import
import sg.beeline.problem._
import sg.beeline.ruinrecreate.{BasicRoutingAlgorithm, BeelineRecreate}
import sg.beeline.util.Util
import sg.beeline.web.SuggestRequest

import scala.util.Try


abstract class RoutingControl
abstract class RoutingNotification

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
      val suggestions : Seq[Suggestion] = settings.dataSource match {
        case "ezlink" => Import.getEzlinkRequests
        case _ => Import.getLiveRequests()
      }
      val suggestionsById = suggestions.map(s => (s.id, s)).toMap
      val modifiedSuggestions = suggestions
        .map({ x => Suggestion(x.id, x.start, x.end, 8 * 3600 * 1000) }) // Group them all into the same time slot

      val beelineProblem = {
        new BasicRoutingProblem(
          busStops,
          modifiedSuggestions,
          startWalkingDistance = settings.startWalkingDistance,
          endWalkingDistance = settings.endWalkingDistance
        )
      }

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
                  // Seed suggestion won't be available
                  suggestionsById.getOrElse(modifiedSuggestion.id, modifiedSuggestion)
                ),
                s)
            case Dropoff(r, s) =>
              val modifiedSuggestion = r
                .asInstanceOf[Request.RequestFromSuggestion]
                .suggestion

              Dropoff(
                new Request.RequestFromSuggestion(
                  r.routingProblem,
                  // Seed suggestion won't be available
                  suggestionsById.getOrElse(modifiedSuggestion.id, modifiedSuggestion)
                ),
                s)
            case a @ _ => a
          }),
          route.time
        )

      val beelineRecreate = new BeelineRecreate(
        beelineProblem,
        beelineProblem.requests
      )(settings)

      beelineRecreate.generatePotentialRoutesFromRequest(
        new Request.RequestFromSuggestion(
          beelineProblem,
          Suggestion(
            0,
            Util.toSVY((sLng, sLat)),
            Util.toSVY((eLng, eLat)),
            8 * 3600e3
          )
        )
      ).toList.map(mapBackSuggestions)
  }

  def receive = {
    case CurrentSolution =>
      sender ! lastResults

    case suggestRequest: SuggestRequest =>
      sender ! Try { routeFromRequest(suggestRequest) }

  }
}

package sg.beeline.jobs

import akka.actor.Actor
import sg.beeline.io.{DataSource, Import}
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

class RouteActor(datasource: DataSource, suggestionSource: String => Seq[Suggestion]) extends Actor {
  var lastResults : Traversable[Route] = List()

  def routeFromRequest(suggestRequest: SuggestRequest) = suggestRequest match {
    case SuggestRequest(sLat, sLng, eLat, eLng, time, settings) =>
      val suggestions : Seq[Suggestion] = suggestionSource(settings.dataSource)
      val suggestionsById = suggestions.map(s => (s.id, s)).toMap

      val beelineProblem = {
        new BasicRoutingProblem(
          suggestions,
          startWalkingDistance = settings.startWalkingDistance,
          endWalkingDistance = settings.endWalkingDistance,
          overrideRouteTime = Some(8 * 3600e3),
          datasource
        )
      }

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
            time
          ),
          8 * 3600e3,
          datasource
        )
      ).toList
  }

  def receive = {
    case CurrentSolution =>
      sender ! lastResults

    case suggestRequest: SuggestRequest =>
      sender ! Try { routeFromRequest(suggestRequest) }

  }
}

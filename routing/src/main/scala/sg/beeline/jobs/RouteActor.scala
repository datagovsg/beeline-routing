package sg.beeline.jobs

import akka.actor.Actor
import sg.beeline.io.{DataSource, Import}
import sg.beeline.problem._
import sg.beeline.ruinrecreate.{BasicRoutingAlgorithm, BeelineRecreate}
import sg.beeline.util.Util
import sg.beeline.web.SuggestRequest

class RouteActor(dataSource: DataSource, suggestionSource: String => Seq[Suggestion]) extends Actor {
  def routeFromRequest(suggestRequest: SuggestRequest) = suggestRequest match {
    case SuggestRequest(sLat, sLng, eLat, eLng, time, settings) =>
      val suggestions : Seq[Suggestion] = suggestionSource(settings.dataSource)

      val beelineProblem = {
        new BasicRoutingProblem(
          suggestions,
          startWalkingDistance = settings.startWalkingDistance,
          endWalkingDistance = settings.endWalkingDistance,
          dataSource
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
            -999, // Some ID that would not occur naturally in the database
            Util.toSVY((sLng, sLat)),
            Util.toSVY((eLng, eLat)),
            time
          ),
          8 * 3600e3,
          dataSource
        )
      ).toList
  }

  def receive = {
    case suggestRequest: SuggestRequest =>
      sender ! routeFromRequest(suggestRequest)
  }
}

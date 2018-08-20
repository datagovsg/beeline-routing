package sg.beeline.jobs

import java.util.concurrent.ForkJoinPool

import akka.actor.Actor
import sg.beeline.io.{DataSource, Import}
import sg.beeline.problem._
import sg.beeline.ruinrecreate.{BasicRoutingAlgorithm, BeelineRecreate, BeelineSuggestRoute}
import sg.beeline.util.Util
import sg.beeline.web.SuggestRequest

import scala.concurrent.ExecutionContext

class RouteActor(dataSource: DataSource, suggestionSource: String => Seq[Suggestion]) extends Actor {
  // If we don't set this, Scalatest hangs when running multiple threads
  implicit val executionContext = ExecutionContext.fromExecutor(
    new ForkJoinPool(Runtime.getRuntime.availableProcessors))

  private def routeFromRequest(suggestRequest: SuggestRequest) = suggestRequest match {
    case SuggestRequest(sLat, sLng, eLat, eLng, time, settings) =>
      val suggestions : Seq[Suggestion] = suggestionSource(settings.dataSource)

      val beelineProblem = new BasicRoutingProblem(
        suggestions,
        startWalkingDistance = settings.startWalkingDistance,
        endWalkingDistance = settings.endWalkingDistance,
        dataSource
      )

      val seedRequest = new Request.RequestFromSuggestion(
        Suggestion(
          -999, // Some ID that would not occur naturally in the database
          Util.toSVY((sLng, sLat)),
          Util.toSVY((eLng, eLat)),
          time
        ),
        beelineProblem,
        dataSource
      )

      val beelineSuggestRoute = new BeelineSuggestRoute(
        beelineProblem,
        beelineProblem.requests
          .filter(suggestRequest.settings.requestsFilter(seedRequest))
          .map(_.withTime(time)),
        settings
      )

      beelineSuggestRoute.generatePotentialRoutesFromRequest(seedRequest).toList
  }

  def receive = {
    case suggestRequest: SuggestRequest =>
      sender ! routeFromRequest(suggestRequest)
  }
}

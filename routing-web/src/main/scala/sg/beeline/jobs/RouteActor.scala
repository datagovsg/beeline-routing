package sg.beeline.jobs

import java.sql.Timestamp
import java.util.concurrent.ForkJoinPool

import akka.actor.Actor
import sg.beeline.io.{BuiltIn, DataSource}
import sg.beeline.problem._
import sg.beeline.ruinrecreate._
import sg.beeline.util.Projections
import sg.beeline.web.SuggestRequest

import scala.concurrent.ExecutionContext
import scala.util.Try

class RouteActor(dataSource: DataSource,
                 suggestionSource: String => Seq[Suggestion],
                 beelineSuggestRouteService: BeelineSuggestRouteService) extends Actor {
  // If we don't set this, Scalatest hangs when running multiple threads
  implicit val executionContext = ExecutionContext.fromExecutor(
    new ForkJoinPool(Runtime.getRuntime.availableProcessors))

  private def routeFromRequest(suggestRequest: SuggestRequest) = suggestRequest match {
    case SuggestRequest(sLat, sLng, eLat, eLng, time, daysOfWeek, settings) =>
      val suggestions : Seq[Suggestion] = suggestionSource(settings.dataSource)

      val seedSuggestion = Suggestion(
        -999, // Some ID that would not occur naturally in the database
        Projections.toSVY((sLng, sLat)),
        Projections.toSVY((eLng, eLat)),
        time,
        createdAt = 0L,
        userId = None,
        email = None,
        daysOfWeek = suggestRequest.daysOfWeek
      )

      val beelineProblem = new BasicRoutingProblem(
        suggestions.filter(suggestRequest.settings.suggestionsFilter(seedSuggestion)),
        dataSource,
        settings = settings,
      )

      val seedRequest = new Request.RequestFromSuggestion(
        seedSuggestion,
        beelineProblem,
        dataSource
      )

      val beelineSuggestRoute = new BeelineSuggestRoute(
        beelineProblem,
        beelineProblem.requests
          .map(_.withTime(time)),
        beelineSuggestRouteService
      )

      Try {
        beelineSuggestRoute.generatePotentialRoutesFromRequest(seedRequest).toList
      }
  }

  def receive = {
    case suggestRequest: SuggestRequest =>
      sender ! routeFromRequest(suggestRequest)
  }
}

package sg.beeline.ruinrecreate

import java.util.concurrent.ForkJoinPool

import io.circe.parser._
import sg.beeline.exc.TooFewSuggestions
import sg.beeline.problem._
import sg.beeline.util.WeightedRandomShuffle

import scala.annotation.tailrec
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.{Failure, Success}

class BeelineSuggestRoute(routingProblem : RoutingProblem,
                          requests: Traversable[Request],
                          beelineSuggestRouteService: BeelineSuggestRouteService)
                         (implicit val executionContext: ExecutionContext) {
  private val settings = routingProblem.settings
  val MAX_DETOUR_MINUTES = settings.maxDetourMinutes
  val START_CLUSTER_RADIUS = settings.startClusterRadius
  val END_CLUSTER_RADIUS = settings.endClusterRadius

  private def isCompatible(r1: Request, r2: Request): Boolean = {
    odCombis(r1).exists({
      case (o, d) =>
        r2.startStops.exists(bs =>
          detourTime((o, d), bs) < MAX_DETOUR_MINUTES * 60000 ||
            detourTime((o, bs), d) < MAX_DETOUR_MINUTES * 60000 ||
            detourTime((bs, o), d) < MAX_DETOUR_MINUTES * 60000
        ) &&
          r2.endStops.exists(bs =>
            detourTime((o, d), bs) < MAX_DETOUR_MINUTES * 60000 ||
              detourTime((o, bs), d) < MAX_DETOUR_MINUTES * 60000 ||
              detourTime((bs, o), d) < MAX_DETOUR_MINUTES * 60000
          )
    })
  }

  private def isCompatible(od: (BusStop, BusStop), bs: BusStop): Boolean = {
    val (o,d) = od

    detourTime((o, d), bs) < MAX_DETOUR_MINUTES * 60000 ||
      detourTime((o, bs), d) < MAX_DETOUR_MINUTES * 60000 ||
      detourTime((bs, o), d) < MAX_DETOUR_MINUTES * 60000
  }

  private def odCombis(request: Request) = {
    for (i <- request.startStops; j <- request.endStops) yield (i, j)
  }


  private def travelTime(s: Seq[BusStop]) =
    s.sliding(2).map({
      case Seq(x, y) =>
        if (x != y)
          routingProblem.distance(x, y) + routingProblem.settings.imputedDwellTime
        else
          0.0
    }).sum

  private def detourTime(ab: (BusStop, BusStop), c: BusStop): Double = {
    val (a, b) = ab

    travelTime(Seq(a, c, b)) - travelTime(Seq(a, b))
  }

  def generatePotentialRoutesFromRequest(request: Request): Traversable[Route2] = {
    println(s"Only ${requests.size} suggestions used")
    println(s"Average # start stops ${requests.map(_.startStops.size).sum / requests.size.toDouble}")
    println(s"Average # end stops ${requests.map(_.endStops.size).sum / requests.size.toDouble}")

    val ods = odCombis(request)

    val compatibleRequests = {
      requests.filter(other => isCompatible(request, other)).toSet
    }

    println(s"${compatibleRequests.size} compatible requests")

    if (compatibleRequests.size < settings.minRequests) {
      throw new TooFewSuggestions
    }

    // You can take the, say, top 5 ODs that has minimum travel
    // time for **this** request, and then grow routes from these 5 ODs
    val top5Ods = ods.sortBy(
      od => routingProblem.distance(od._1, od._2)
    ).take(5)

    val feasibleTop50Ods = top5Ods.filter(
      od => {
        val topOd = top5Ods(0)
        routingProblem.distance(od._1, od._2) - routingProblem.distance(topOd._1, topOd._2) <
          routingProblem.settings.suboptimalStopChoiceAllowance
      }
    )

    val feasible = feasibleTop50Ods.flatMap(od => {
      (0 until 10)
        .map(i => (i, od))
    })

    /**
      * Obviously we don't need 50 threads to make 50 HTTP requests. Problem is that AWS lambda library
      * has a Java Future, which instead of Future.traverse requires an "ExecutorCompletionService")
      */
    implicit val highlyParallelExecutionContext = ExecutionContext.fromExecutor(new ForkJoinPool(50))

    val feasibleTop50Routes = Await.result(Future.traverse(feasible){ case (i, od) =>
      beelineSuggestRouteService.executeLambda(settings, request, od, compatibleRequests.toList)
    }, Duration.Inf)

    // Prepend candidateRoute to uniqueRoutes if it is different from all the routes in uniqueRoutes
    def buildNext(uniqueRoutes : List[Route2], candidateRoute : Route2) = {
      val (similarRoutes, dissimilarRoutes) =
        uniqueRoutes.partition(route => RouteSimilarity.isSimilar2(route, candidateRoute, settings.similarityLimit))
      (candidateRoute :: similarRoutes).maxBy(_.requests.size) :: dissimilarRoutes
    }

    val uniqueTop50Routes = feasibleTop50Routes.foldLeft(List[Route2]())(buildNext)

    println(s"Removed ${feasibleTop50Routes.size - uniqueTop50Routes.size} similar routes")

    uniqueTop50Routes
      .groupBy(_.stops)
      .values.map(_.head)
      .toList
  }

  def growRoute(request : Request, od : (BusStop, BusStop), requests: List[Request]) = {
    val shuffled = WeightedRandomShuffle.shuffle(requests, requests.view.map(r => r.weight.toDouble))
      .toList

    // Try to insert until a max of.... 10?
    @tailrec
    def grow(route : Route2, requests : List[Request]) : Route2 = {
      if (requests.isEmpty)
        route
      else {
        grow(
          route.jobTryInsertion(requests.head)(settings).getOrElse(route),
          requests.tail
        )
      }
    }

    grow(
      new Route2(
        routingProblem
      )(
        Array((od._1, List(request))),
        Array((od._2, List(request)))
      ),
      shuffled
    )
  }
}

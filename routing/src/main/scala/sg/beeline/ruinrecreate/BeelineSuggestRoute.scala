package sg.beeline.ruinrecreate

import java.util.concurrent.ForkJoinPool

import io.circe.parser._
import sg.beeline.problem._
import sg.beeline.ruinrecreate.BeelineSuggestRouteService.{OD, SuggestRouteInput}
import sg.beeline.util.WeightedRandomShuffle

import scala.annotation.tailrec
import scala.collection.parallel.ForkJoinTaskSupport
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

class BeelineSuggestRoute(routingProblem : RoutingProblem,
                          requests: Traversable[Request],
                          beelineSuggestRouteService: BeelineSuggestRouteService)
                         (implicit val executionContext: ExecutionContext) {
  private val settings = routingProblem.settings
  val MAX_DETOUR_MINUTES = settings.maxDetourMinutes
  val START_CLUSTER_RADIUS = settings.startClusterRadius
  val END_CLUSTER_RADIUS = settings.endClusterRadius

  println(s"Only ${requests.size} suggestions used")
  println(s"Average # start stops ${requests.map(_.startStops.size).sum / requests.size.toDouble}")
  println(s"Average # end stops ${requests.map(_.endStops.size).sum / requests.size.toDouble}")

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
          routingProblem.distance(x, y) + 60000.0
        else
          0.0
    }).sum

  private def detourTime(ab: (BusStop, BusStop), c: BusStop): Double = {
    val (a, b) = ab

    travelTime(Seq(a, c, b)) - travelTime(Seq(a, b))
  }

  def generatePotentialRoutesFromRequest(request: Request): Traversable[Route] = {
    val ods = odCombis(request)

    val compatibleRequests = {
      requests.filter(other => isCompatible(request, other)).toSet
    }

    println(s"${compatibleRequests.size} compatible requests")

    // You can take the, say, top 5 ODs that has minimum travel
    // time for **this** request, and then grow routes from these 5 ODs
    val top5Ods = ods.sortBy(
      od => routingProblem.distance(od._1, od._2)
    ).take(5)

    val feasibleTop50Ods = top5Ods.filter(
      od => {
        val topOd = top5Ods(0)
        routingProblem.distance(od._1, od._2) - routingProblem.distance(topOd._1, topOd._2) < 60000
      }
    )

    val feasible = feasibleTop50Ods.flatMap(od => {
      (0 until 10)
        .map(i => (i, od))
    })

    val feasibleTop50Routes = {
      val p = feasible.par
      p.tasksupport = new ForkJoinTaskSupport(new ForkJoinPool(10))
      p
    }.flatMap { case (i, od) =>
      beelineSuggestRouteService.executeInput(
        SuggestRouteInput(settings, request, OD(od._1, od._2), compatibleRequests.toList)
      ).toOption
    }

    // Prepend candidateRoute to uniqueRoutes if it is different from all the routes in uniqueRoutes
    def buildNext(uniqueRoutes : List[Route], candidateRoute : Route) = {
      val (similarRoutes, dissimilarRoutes) = uniqueRoutes.partition(route => RouteSimilarity.isSimilar(route, candidateRoute))
      (candidateRoute :: similarRoutes).maxBy(_.requestsInfo.size) :: dissimilarRoutes
    }

    val uniqueTop50Routes = feasibleTop50Routes.foldLeft(List[Route]())(buildNext)

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
    def grow(route : Route, requests : List[Request]) : Route = {
      if (requests.isEmpty)
        route
      else {
        val insertAttempt = route.jobTryInsertion(requests.head)(settings.maxDetourMinutes * 60000)

        /* Here we should check how feasible the route is, e.g. if
        the travel time is too long for any commuter in the route.
        If so stop the recursion at this point.
         */
        insertAttempt match {
          case None =>
            grow(route, requests.tail)
          case Some(insertionJob) =>
            val (a,b,c,d,e) = insertionJob
            val newRoute = route.insert(b,c,d,e)

            if (newRoute.maxDetour > 15 * 60000)
              grow(route, requests.tail)
            else
              grow(route.insert(b,c,d,e), requests.tail)
        }
      }
    }

    grow(
      new Route(
        routingProblem,
        IndexedSeq(
          StartActivity(),
          Pickup(request, od._1),
          Dropoff(request, od._2),
          EndActivity()
        ),
        request.time
      ),
      shuffled
    )
  }
}

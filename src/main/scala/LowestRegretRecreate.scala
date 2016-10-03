package sg.beeline

import scala.annotation.tailrec
import scala.util.Random
import scala.collection.immutable.HashMap

object LowestRegretRecreate extends Recreate {
  type Insertion = (Double, Activity, Activity, (Activity, Activity), (Activity, Activity))

  var count : Int = 0
  var costCache : Map[Request, Map[Route, Insertion]] = new HashMap
  var costCacheMutex = new Object

  def tryCreateRoute(problem : RoutingProblem)(request : Request) = {
    // Construct new route
    val randomPickup = new Pickup(request, request.startStops({
      Random.nextInt % request.startStops.size
    }))
    val randomDropoff = new Dropoff(request, request.endStops({
      Random.nextInt % request.endStops.size
    }))

    // Check if it's possible...
    if (Route.distCost(problem)(randomPickup.location, randomDropoff.location) == Double.PositiveInfinity)
      None
    else
      Some(new Route(
        problem,
        List(new StartActivity, randomPickup, randomDropoff, new EndActivity),
        request.time
      ))
  }

  private def computeRegret(route : Route, request : Request) : Insertion = {
    route.jobTryInsertion(request) match {
      case None => (Double.PositiveInfinity, null, null, null, null)
      case Some(insertion) => {
        insertion
      }
    }
  }

  private def getRegret(route : Route, request : Request) : Insertion = {

    costCache.get(request) match {
      case Some(routeCache) =>
        routeCache.get(route) match {
        case Some(regret) => {
          regret
        }
        case None => computeRegret(route, request)
      }
      case None => computeRegret(route, request)
    }
  }

  def recreate(problem : RoutingProblem, preservedRoutes : Traversable[Route], unservedRequests : Traversable[Request]) = {
    @tailrec
    def next(unservedRequests: Set[Request], routes: Set[Route], badRequests: List[Request])
    : (Set[Route], List[Request]) = {
      if (count % 100 == 0) {
//        println((routes.size, unservedRequests.size, badRequests.size))
        println(count)
      }

      count += 1

      if (unservedRequests.isEmpty) (routes, badRequests)
      else {
        // Get the minimum regret
        // Use a lazy to avoid choking on empty route list
        lazy val best = {
          // For all requests, compute the regret
          val requestCosts = unservedRequests.par.map(req => {
            val routeCosts = routes.map(route => (route, getRegret(route, req)))
            val minRouteCosts = routeCosts.minBy(_._2._1)

            costCacheMutex.synchronized({
              costCache = costCache + (req -> routeCosts.toMap)
            })

            (req, minRouteCosts)
          })

          requestCosts.minBy(_._2._2._1)
        }

        if (routes.isEmpty || best._2._2._1 > 5 * 60000 /*== Double.PositiveInfinity*/) {
          val someRequest = unservedRequests.head

          tryCreateRoute(problem)(someRequest) match {
            // Couldn't create
            case None => next(unservedRequests - someRequest, routes, someRequest::badRequests)
            case Some(route) => next(unservedRequests - someRequest, routes + route, badRequests)
          }
        }
        else {
          val (request, (route, insertion)) = best
          costCache = costCache.mapValues(_ - route)

          next(
            unservedRequests - request,
            routes - route +
              route.insert(insertion._2, insertion._3, insertion._4, insertion._5),
            badRequests
          )
        }
      }
    }

    val requestSet = unservedRequests.toSet
    val routeSet = preservedRoutes.toSet

    // Routes have changed... remove the non-existent routes from the cache
    costCache = costCache.mapValues(h => h -- h.keys.filterNot(r => routeSet.contains(r)))

    next(unservedRequests.toSet, preservedRoutes.toSet, List[Request]()) match {
      case (a,b) => (a.toList, b.toList)
    }
  }
}

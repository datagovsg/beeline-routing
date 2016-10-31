package sg.beeline

import scala.annotation.tailrec
import scala.util.Random
import scala.collection.immutable.HashMap

class BeelineRecreate(routingProblem : RoutingProblem, requests: Traversable[Request]) extends Recreate {
  type Insertion = (Double, Activity, Activity, (Activity, Activity), (Activity, Activity))

  var count : Int = 0
  var costCache : Map[Route, Map[Request, Insertion]] = new HashMap
  var costCacheMutex = new Object

  // Generate all the possible requests
  // Get a map of requests -> compatible requests
  val relatedRequests = {
    val requestsView = requests.view

    def isCompatible(r1: Request, r2: Request) : Boolean = {
//      odCombis(r1).exists(od1 =>
//        odCombis(r2).exists(od2 => detourTime(od1, od2) < 1.6 * 60000))
      odCombis(r1).exists({
        case (o,d) =>
          (r2.startStops.exists(bs => detourTime((o,d), bs) < 1.6 * 60000) &&
            r2.endStops.exists(bs => detourTime((o,d), bs) < 1.6 * 60000)) ||
            (r2.startStops.exists(bs => detourTime((o,bs), d) < 1.6 * 60000) &&
              r2.endStops.exists(bs => detourTime((o,bs), d) < 1.6 * 60000))
      })
    }

    requestsView.par.map(request => {
      (request,
        requestsView.filter(otherRequest => isCompatible(request, otherRequest)).toSet)
    }).toMap
  }
  println("Computed pairwise compatible requests")
  println(("Average number of compatible", relatedRequests.size, relatedRequests.values.map(_.size).sum / relatedRequests.size.toDouble))

  def odCombis(request : Request) = {
    for (i <- request.startStops; j <- request.endStops) yield (i, j)
  }


  def travelTime(s : Seq[BusStop]) =
    s.sliding(2).map({
      case Seq(x, y) =>
        if (x != y)
          routingProblem.distance(x, y) + 60000.0
        else
          0.0
    }).sum

  def detourTime(ab: (BusStop, BusStop), c: BusStop) : Double = {
    val (a,b) = ab

    travelTime(Seq(a,c,b)) - travelTime(Seq(a,b))
  }

  def detourTime(ab: (BusStop, BusStop), cd: (BusStop, BusStop)) : Double = {
    val (a,b) = ab
    val (c,d) = cd

    val abTime = travelTime(Seq(a, b))
    val cdTime = travelTime(Seq(c, d))

    // N.B. Here we ignore the possibility that a->b ->c->d
    val minDetourTime = Seq(
      travelTime(Seq(a, c, d, b)) - abTime, // cd embedded within ab
      travelTime(Seq(c, a, b, d)) - cdTime, // ab embedded within cd
      Seq(travelTime(Seq(a, c, b)) - abTime, travelTime(Seq(c, b, d)) - cdTime).max, // max detour encountered
      Seq(travelTime(Seq(c, a, d)) - cdTime, travelTime(Seq(a, d, b)) - abTime).max // max detour encountered
    ).min

    minDetourTime
  }


  def tryCreateRoute(problem : RoutingProblem)(request : Request) = {
    // Construct new route
    val randomPickup = new Pickup(request, request.startStops({
      Random.nextInt(request.startStops.size)
    }))
    val randomDropoff = new Dropoff(request, request.endStops({
      Random.nextInt(request.endStops.size)
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

    costCache.get(route) match {
      case Some(requestCache) =>
        requestCache.get(request) match {
          case Some(regret) => {
            regret
          }
          case None => computeRegret(route, request)
        }
      case None => computeRegret(route, request)
    }
  }

  private def routeODs(route: Route) = {
    val pickups = route.activities.flatMap({
      case Pickup(request, location) =>
        Some((request, location))
      case _ =>
        None
    }).toMap

    val dropoffs = route.activities.flatMap({
      case Dropoff(request, location) =>
        Some((request, location))
      case _ => None
    }).toMap

    pickups.keys
      .map(request => (pickups.get(request).orNull, dropoffs.get(request).orNull))
  }

  def recreate(problem : RoutingProblem, preservedRoutes : Traversable[Route], unservedRequests : Traversable[Request]) =
  {
    require(problem == routingProblem)

    // For all routes -- create a hash map (BusStop, BusStop) -> List(Route)
    def computeFeasibleSet(route : Route) = {
      val ods = routeODs(route)

      // Reduce to a small set of feasible ods
      val feasible = route.requestsInfo.keys.foldLeft(
        relatedRequests(route.requestsInfo.keys.head)
      ) ((acc, current) =>
        acc.intersect(relatedRequests(current))
      )

      feasible
    }

    val routeOdMap = {
      preservedRoutes.map(route => (route, computeFeasibleSet(route))).toMap
    }

    val odRouteMap = {
      preservedRoutes.flatMap(route => {
        val pickups = route.activities.flatMap({
          case Pickup(request, location) =>
            Some((request, location))
          case _ =>
            None
        }).toMap

        val dropoffs = route.activities.flatMap({
          case Dropoff(request, location) =>
            Some((request, location))
          case _ => None
        }).toMap

        val ods = pickups.keys
          .map(request => (pickups.get(request).orNull, dropoffs.get(request).orNull))

        ods.map(od => (od, route))
      })
        .groupBy(_._1)
        .map({case(a, b) => (a, b.toList.map(_._2))})
    }

    @tailrec
    def next(unservedRequests: Set[Request], routes: Set[Route], badRequests: List[Request],
             routeOdMap: Map[Route, Set[Request]])
    : (Set[Route], List[Request]) = {

      if (count % 100 == 0) {
        println(count)
        println(("Average number of feasible", routeOdMap.values.map(_.size).sum / routeOdMap.size.toDouble))
      }
      count += 1

      if (unservedRequests.isEmpty) (routes, badRequests)
      else {
        // Get the minimum regret
        // Use a lazy to avoid choking on empty route list
        lazy val best = {
          // For all requests, compute the regret
          val requestStops = unservedRequests
          val insertionCosts = routeOdMap.par.flatMap({
            case (route, compatibleSet) =>
              val feasibleRequests = requestStops.filter(r => compatibleSet.contains(r))

              if (feasibleRequests.isEmpty)
                None
              else
                Some({
                  val regrets = feasibleRequests.map(req => (route, req, getRegret(route, req)))

                  /* Update the cache! */
                  {
                    val update = regrets.map {case (route, request, regret) => (request, regret)}

                    costCacheMutex.synchronized({
                      costCache = costCache + (route -> (costCache.getOrElse(route, new HashMap) ++ update))
                    })
                  }

                  regrets.minBy(_._3._1)
                })
          })

          if (insertionCosts.isEmpty)
            None
          else
            Some(insertionCosts.minBy(_._3._1))
        }

        if (routes.isEmpty || best.isEmpty || best.orNull._3._1 == Double.PositiveInfinity) {
          val someRequest = unservedRequests.head
          val newRoute = tryCreateRoute(problem)(someRequest)

          tryCreateRoute(problem)(someRequest) match {
            // Couldn't create
            case None => next(unservedRequests - someRequest,
                              routes,
                              someRequest::badRequests,
                              routeOdMap)
            case Some(route) =>
              next(
                unservedRequests - someRequest,
                routes + route,
                badRequests,
                routeOdMap + (route -> computeFeasibleSet(route))
                )
          }
        }
        else {
          val (oldRoute, request, insertion) = best.orNull
          val pickup = insertion._2 match {case Pickup(_, loc) => loc}
          val dropoff = insertion._3 match {case Dropoff(_, loc) => loc}

          val newRoute = oldRoute.insert(insertion._2, insertion._3, insertion._4, insertion._5).tweak

          costCacheMutex.synchronized({
            costCache = costCache - oldRoute
          })

          val newRouteSet = routes - oldRoute + newRoute
          val newRequestRouteMap = routeOdMap - oldRoute + (newRoute -> computeFeasibleSet(newRoute))

          next(
            unservedRequests - request,
            newRouteSet,
            badRequests,
            newRequestRouteMap
          )
        }
      }
    }

    val requestSet = unservedRequests.toSet
    val routeSet = preservedRoutes.toSet

    // Most routes have been destroyed anyway, so just reset the cache
    costCache = new HashMap

    next(unservedRequests.toSet, preservedRoutes.toSet, List[Request](), routeOdMap) match {
      case (a,b) => (a.toList, b.toList)
    }
  }

}


package sg.beeline

import scala.annotation.tailrec
import scala.util.Random
import scala.collection.immutable.HashMap

class BeelineRecreate(routingProblem : RoutingProblem, requests: Traversable[Request]) extends Recreate {
  type Insertion = (Double, Activity, Activity, (Activity, Activity), (Activity, Activity))

  var count : Int = 0
  var costCache : Map[Request, Map[Route, Insertion]] = new HashMap
  var costCacheMutex = new Object

  // Generate all the possible requests
  // Get a map of requests -> compatible requests
  val relatedRequests = {
    val possibleStops = requests.flatMap(request => odCombis(request))
      .toSet // Make it unique

    possibleStops.map({
      case (i,j) =>
        ((i,j) -> possibleStops.filter({
          case (k,l) =>
            detourTime((i,j), (k,l)) < 15 * 60000
        }))
    })
      .toMap
  }
  println("Computed pairwise compatible requests")

  def odCombis(request : Request) = {
    for (i <- request.startStops; j <- request.endStops) yield (i, j)
  }

  def detourTime(ab: (BusStop, BusStop), cd: (BusStop, BusStop)) : Double = {
    val (a,b) = ab
    val (c,d) = cd
    val abTime = routingProblem.distance(a, b)
    val cdTime = routingProblem.distance(c, d)

    def travelTime(s : Seq[BusStop]) =
      s.sliding(2).map({
        case Seq(x, y) =>
          if (x != y)
            routingProblem.distance(x, y) + 60000.0
          else
            0.0
      })
        .sum

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

  def recreate(problem : RoutingProblem, preservedRoutes : Traversable[Route], unservedRequests : Traversable[Request]) =
  {
    require(problem == routingProblem)

    // For all routes -- create a hash map (BusStop, BusStop) -> List(Route)
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
             requestRouteMap: Map[(BusStop, BusStop), List[Route]])
    : (Set[Route], List[Request]) = {

      if (count % 100 == 0) {
        //        println((routes.size, unservedRequests.size, badRequests.size))
        println(count)
      }
      count += 1

      if (unservedRequests.isEmpty) (routes, badRequests)
      else {
        // For all request -> (geo) List(BusStop, BusStop)
        //  -> (compatible) List(BusStop, BusStop)
        //  -> (hash map) ListRoutes

        // Get the minimum regret
        // Use a lazy to avoid choking on empty route list
        lazy val best = {
          // For all requests, compute the regret
          val requestCosts = unservedRequests.par.map(req => {
            val candidateRoutes = odCombis(req).flatMap(od => /* Find possible OD combis */
                relatedRequests.get(od).orNull.flatMap(od => /* compatible OD combis */
                  requestRouteMap.get(od) match {
                    case Some(routes) => routes
                    case None => List()
                  })) /* Routes */

            val routeCosts = candidateRoutes.map(route => (route, getRegret(route, req)))
            val minRouteCosts = routeCosts.minBy(_._2._1)

            costCacheMutex.synchronized({
              costCache = costCache + (req -> routeCosts.toMap)
            })

            (req, minRouteCosts)
          })

          if (requestCosts.isEmpty)
            None
          else
            Some(requestCosts.minBy(_._2._2._1))
        }

        if (routes.isEmpty || best.isEmpty || best.orNull._2._2._1 == Double.PositiveInfinity) {
          val someRequest = unservedRequests.head
          val newRoute = tryCreateRoute(problem)(someRequest)

          tryCreateRoute(problem)(someRequest) match {
            // Couldn't create
            case None => next(unservedRequests - someRequest,
                              routes,
                              someRequest::badRequests,
                              requestRouteMap)
            case Some(route) =>
              val pickup = route.activities(1) match {case Pickup(_, loc) => loc}
              val dropoff = route.activities(2) match {case Dropoff(_, loc) => loc}

              next(
                unservedRequests - someRequest,
                routes + route,
                badRequests,
                requestRouteMap + ((pickup, dropoff) -> List(route))
                )
          }
        }
        else {
          val (request, (oldRoute, insertion)) = best.orNull
          val pickup = insertion._2 match {case Pickup(_, loc) => loc}
          val dropoff = insertion._3 match {case Dropoff(_, loc) => loc}

          val newRoute = oldRoute.insert(insertion._2, insertion._3, insertion._4, insertion._5)

          costCache = costCache.mapValues(_ - oldRoute)

          val newRouteSet = routes - oldRoute + newRoute
          val newRequestRouteMap = requestRouteMap.mapValues(routes =>
            routes.filter(_ != oldRoute))

          next(
            unservedRequests - request,
            newRouteSet,
            badRequests,
            newRequestRouteMap + ((pickup, dropoff) -> { /* Update the map... */
              newRequestRouteMap.get((pickup, dropoff)) match {
                case None => List(newRoute)
                case Some(routes) => newRoute :: routes
              }
            })
          )
        }
      }
    }


    val requestSet = unservedRequests.toSet
    val routeSet = preservedRoutes.toSet

    // Routes have changed... remove the non-existent routes from the cache
    costCache = costCache.mapValues(h => h -- h.keys.filterNot(r => routeSet.contains(r)))

    next(unservedRequests.toSet, preservedRoutes.toSet, List[Request](), odRouteMap) match {
      case (a,b) => (a.toList, b.toList)
    }
  }

}


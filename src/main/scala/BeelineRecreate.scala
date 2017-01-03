package sg.beeline

import com.thesamet.spatial.{KDTreeMap, KDTree}

import scala.annotation.tailrec
import scala.util.Random
import scala.collection.immutable.HashMap
import kdtreeQuery.KDTreeMapBall

class BeelineRecreate(routingProblem : RoutingProblem, requests: Traversable[Request])
                     (implicit settings : BeelineRecreateSettings = BeelineRecreateSettings.default) extends Recreate {
  type Insertion = (Double, Activity, Activity, (Activity, Activity), (Activity, Activity))

  var count: Int = 0
  var costCache: Map[Route, Map[Request, Insertion]] = new HashMap
  var costCacheMutex = new Object

  var MAX_DETOUR_MINUTES = settings.maxDetourMinutes
  var START_CLUSTER_RADIUS = settings.startClusterRadius
  var END_CLUSTER_RADIUS = settings.endClusterRadius

  // KD trees
  val startStopTree = KDTreeMap.fromSeq(
    requests.map({ r => (r.start, r) }).toSeq
  )
  val endStopTree = KDTreeMap.fromSeq(
    requests.map({ r => (r.end, r) }).toSeq
  )
  val busStopTree = KDTreeMap.fromSeq(
    Import.getBusStops.map(bs => (bs.xy, bs))
  )

  def isCompatible(r1: Request, r2: Request): Boolean = {
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

  def isCompatible(od: (BusStop, BusStop), bs: BusStop): Boolean = {
    val (o,d) = od

    detourTime((o, d), bs) < MAX_DETOUR_MINUTES * 60000 ||
      detourTime((o, bs), d) < MAX_DETOUR_MINUTES * 60000 ||
      detourTime((bs, o), d) < MAX_DETOUR_MINUTES * 60000
  }
  // Generate all the possible requests
  // Get a map of requests -> compatible requests
  lazy val relatedRequests = {
    val requestsView = requests.view

    val map = requestsView.par.map(request => {
      val compatibleRequests = startStopTree.queryBall(request.start, START_CLUSTER_RADIUS).map(_._2).intersect(
        endStopTree.queryBall(request.end, END_CLUSTER_RADIUS).map(_._2)
      )
        .filter(other => isCompatible(request, other)).toSet

      (request, compatibleRequests)
    }).toMap

    println("Computed pairwise compatible requests")
    println(("Average number of compatible", map.size, map.values.map(_.size).sum / map.size.toDouble))

    map
  }

  def odCombis(request: Request) = {
    for (i <- request.startStops; j <- request.endStops) yield (i, j)
  }


  def travelTime(s: Seq[BusStop]) =
    s.sliding(2).map({
      case Seq(x, y) =>
        if (x != y)
          routingProblem.distance(x, y) + 60000.0
        else
          0.0
    }).sum

  def detourTime(ab: (BusStop, BusStop), c: BusStop): Double = {
    val (a, b) = ab

    travelTime(Seq(a, c, b)) - travelTime(Seq(a, b))
  }

  def detourTime(ab: (BusStop, BusStop), cd: (BusStop, BusStop)): Double = {
    val (a, b) = ab
    val (c, d) = cd

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


  def tryCreateRoute(problem: RoutingProblem)(request: Request) = {
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

  private def computeRegret(route: Route, request: Request): Insertion = {
    route.jobTryInsertion(request)(MAX_DETOUR_MINUTES * 60000) match {
      case None => (Double.PositiveInfinity, null, null, null, null)
      case Some(insertion) => {
        insertion
      }
    }
  }

  private def getRegret(route: Route, request: Request): Insertion = {
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
      .toSet
  }

  def recreate(problem: RoutingProblem, preservedRoutes: Traversable[Route], unservedRequests: Traversable[Request]) = {
    require(problem == routingProblem)

    // For all routes -- create a hash map (BusStop, BusStop) -> List(Route)
    def computeFeasibleSet(route: Route) = {
      // Reduce to a small set of feasible ods
      val feasible = route.requestsInfo.keys.foldLeft(
        relatedRequests(route.requestsInfo.keys.head)
      )((acc, current) =>
        acc.intersect(relatedRequests(current))
      )

      feasible
    }

    val routeOdMap = {
      preservedRoutes.map(route => (route, computeFeasibleSet(route))).toMap
    }

    @tailrec
    def next(unservedRequests: Set[Request], routes: Set[Route], badRequests: List[Request],
             routeOdMap: Map[Route, Set[Request]])
    : (Set[Route], List[Request]) = {

      if (count % 500 == 0) {
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
              val feasibleRequests = requestStops.intersect(compatibleSet)

              if (feasibleRequests.isEmpty)
                None
              else
                Some({
                  val regrets = feasibleRequests.map(req => (route, req, getRegret(route, req)))

                  /* Update the cache! */
                  {
                    val update = regrets.map { case (_, request, regret) => (request, regret) }

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
          // Create a new route from any request

          val someRequest = unservedRequests.head
          val newRoute = tryCreateRoute(problem)(someRequest)

          tryCreateRoute(problem)(someRequest) match {
            // Couldn't create
            case None => next(unservedRequests - someRequest,
              routes,
              someRequest :: badRequests,
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
      case (a, b) => (a.toList, b.toList)
    }
  }


  def findRelated(request: Request): Traversable[Route] = {
    val ods = odCombis(request)

    // compatible requests... --> reduce your search space
    val compatibleRequests = {
      val startStopTree = KDTreeMap.fromSeq(
        requests.map({ r => (r.start, r) }).toSeq
      )
      val endStopTree = KDTreeMap.fromSeq(
        requests.map({ r => (r.end, r) }).toSeq
      )

      startStopTree.queryBall(request.start, START_CLUSTER_RADIUS).map(_._2).intersect(
        endStopTree.queryBall(request.end, END_CLUSTER_RADIUS).map(_._2)
      )
        .filter(other => isCompatible(request, other)).toSet
    }

    println(s"There are ${compatibleRequests.size} compatible requests")

    // list of (s, e)
    val odsWithRoute = ods.map(od => {
      val route = new Route(routingProblem,
        List(StartActivity(), Pickup(request, od._1), Dropoff(request, od._2), EndActivity()),
        8 * 3600 * 1000)

      (route, // fake the time
        IndexedSeq(request), // matched
        compatibleRequests.toIndexedSeq // unmatched
//        route.maxPossibleTimes(2) - route.maxPossibleTimes(1)
      )
    })
//      .sortBy(_._4)

    // Don't allow ODs that obviously end up taking a very long time
//    val feasibleOdsWithRoute = odsWithRoute
//      .takeWhile({
//        case (_,_,_,duration) => duration - odsWithRoute(0)._4 < 60000
//      })

    type Entry = (Route, IndexedSeq[Request], IndexedSeq[Request])

    // list of ((s, e), R([s, e]), [r1])
    // add compatible requests to it...
    // and discard incompatible requests
    def addMatchingRequests(entry: Entry) =
      entry match {
        case (route, rs, rest) =>
          val (served, unserved, incompatible) = rest
            .foldLeft[(List[Request], List[Request], List[Request])](
            List(), List(), List()
          )((acc, r) => acc match {
            case (served, unserved, incompatible) =>
              // I am going to cheat a bit...
              // FIXME: Let each route contain the list of pickup and dropoff stops
              if (route.stopsWithIndices.exists(x => r.startStopsSet.contains(x)) &&
                route.stopsWithIndices.exists(x => r.endStopsSet.contains(x)))
                (r :: served, unserved, incompatible)
              else {
                // CHeck if this request is compatible with ALL of the requests in the route
                // FIXME: Add it to a map for quicker access
                val isCompatibleWithAll = route.requestsInfo.keys.forall(s => isCompatible(r, s))

                if (isCompatibleWithAll)
                  (served, r :: unserved, incompatible)
                else
                  (served, unserved, r :: incompatible)
              }
          })

          (route, rs ++ served.toIndexedSeq, unserved.toIndexedSeq)
      }

    val withMatchingStops = odsWithRoute map addMatchingRequests

    // list of ((s, e), R([s, e]), [r1, r2, r3, r4, ...])
    // Now we need to grow the list of candidate routes
    def growOne(entry: Entry): Traversable[Entry] = {
      // reduce the list of compatible requests

      // from the list of requests (or ODs ?)
      // insert the OD into the job...
      @tailrec
      def next(entry: Entry,
               nextRequestCandidates: Set[Request],
               acc: IndexedSeq[Entry]): IndexedSeq[Entry] = {
        if (nextRequestCandidates.isEmpty)
          acc
        else {
          // pick a random request
          val route = entry._1
          val randomRequest = nextRequestCandidates.drop(Random.nextInt(nextRequestCandidates.size)).head
          val insertAttempt = route.jobTryInsertion(randomRequest)

          insertAttempt match {
            case None =>
              next(entry, nextRequestCandidates - randomRequest, acc) // drop the useless candidate

            case Some((cost, a1, a2, ip1, ip2)) =>
              val newRoute = route.insert(a1, a2, ip1, ip2)
              val newEntry = addMatchingRequests((newRoute, entry._2, entry._3))

              next(entry, nextRequestCandidates -- newEntry._2, newEntry +: acc) // pick from the remaining unserved requests
          }
        }
      }
      next(entry, entry._3.toSet, IndexedSeq())
    }

    def growAll(in: Traversable[Entry]): Traversable[Entry] =
      in.par.flatMap(e => {
        val grown = growOne(e)
        if (grown.isEmpty)
          IndexedSeq((e._1, e._2, IndexedSeq())) // Don't allow e to grow any more
        else
          grown
      })
        .toIterator
        .map(x => (x._1.stopsWithIndices.toSet, x)) // distinct by route...
        .toMap
        .values

    // grow ten times...
    val grownTenTimes = (0 until 10).foldLeft[Traversable[Entry]](
      withMatchingStops
    ) { (acc, x) => {

      println(acc.size)
      growAll(acc)

    }
    }

    grownTenTimes.map {
      _._1
    }
  }

  def findRelated2(request: Request): Traversable[Route] = {
    val ods = odCombis(request)

    val compatibleRequests = {
      val startStopTree = KDTreeMap.fromSeq(
        requests.map({ r => (r.start, r) }).toSeq
      )
      val endStopTree = KDTreeMap.fromSeq(
        requests.map({ r => (r.end, r) }).toSeq
      )

      startStopTree.queryBall(request.start, START_CLUSTER_RADIUS).map(_._2).intersect(
        endStopTree.queryBall(request.end, END_CLUSTER_RADIUS).map(_._2)
      )
        .filter(other => isCompatible(request, other)).toSet
    }

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

    feasibleTop50Ods.par.flatMap(od => {
      (0 until 10)
        .flatMap(i => try {
          val r = growRoute(request, od, compatibleRequests.toList)
          println(s"Route generated from ${  compatibleRequests.size} requests")
          Some(r)
        } catch {
          case e : Exception => println(e); None
        })
    })
      .groupBy(_.stops)
      .values.map(_.head)
      .toList
  }

  def growRoute(request : Request, od : (BusStop, BusStop), requests: List[Request]) = {
    // val shuffled = Random.shuffle(requests)
    val shuffled = WeightedRandomShuffle.shuffle(requests, requests.view.map(r => r.weight.toDouble))

    // Try to insert until a max of.... 10?
    @tailrec
    def grow(route : Route, requests : List[Request]) : Route = {
      if (requests.isEmpty)
        route
      else {
        val insertAttempt = route.jobTryInsertion(requests.head)

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
        8*3600*1000
      ),
      shuffled
    )
  }
}


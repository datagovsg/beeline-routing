package sg.beeline

import scala.annotation.tailrec
import scala.collection.immutable.HashMap
import Route.groupSuccessive

object Route {
  def empty(routingProblem: RoutingProblem, time: Double) =
    new Route(routingProblem, List(new StartActivity, new EndActivity), time)

  def distCost(routingProblem: RoutingProblem) (optLoc1: Option[BusStop], optLoc2: Option[BusStop]) =
    (optLoc1, optLoc2) match {
      case (Some(l1), Some(l2)) => routingProblem.distance(l1, l2) // Distance is already in seconds
      case _ => 0.0
    }

  // FIXME: fix the tail recursion bit
  def groupSuccessive[A, B]
  (t : Traversable[A])(fn : A => B) : List[(B, Traversable[A])] = {
    if (t.isEmpty)
      List()
    else {
      val first = fn(t.head)
      val (same, different) = t.span(x => fn(x) == first)

      (first, same) :: groupSuccessive(different)(fn)
    }
  }
}

class Route(val routingProblem: RoutingProblem,
            _activities : Seq[Activity],
            val time: Double) {

  val activities = _activities.toIndexedSeq

  // Cost between two points. Accepts options. Returns 0 if either is None
  def distCost(optLoc1: Option[BusStop], optLoc2: Option[BusStop]) = Route.distCost(routingProblem)(optLoc1, optLoc2)

  def startTimeDifference(a1 : Activity, a2: Activity) =
    (if (a1.location == a2.location) 0 else a1.dwellTime) +
    a1.serviceTime +
    distCost(a1.location, a2.location)
  /* Compute the minimum possible service time */

  /* Compute the maximum possible service time */
  val (minPossibleTimes, maxPossibleTimes) = {
    val len = activities.size
    val minTimes = Array.ofDim[Double](len)
    val maxTimes = Array.ofDim[Double](len)

    minTimes(0) = activities(0).minTime
    maxTimes(len - 1) = activities(len - 1).maxTime

    for (i <- 1 until len) {
      val lastMinTime = minTimes(i - 1)
      val lastActivity = activities(i - 1)
      val currentActivity = activities(i)

      val timeFromPrevious = lastMinTime + startTimeDifference(lastActivity, currentActivity)

      minTimes(i) = Math.max(timeFromPrevious, currentActivity.minTime)
    }

    for (i <- (len - 2) to 0 by -1) {
      val nextMaxTime = maxTimes(i + 1)
      val nextActivity = activities(i + 1)
      val currentActivity = activities(i)

      val timeFromNext = nextMaxTime - startTimeDifference(currentActivity, nextActivity)

      maxTimes(i) = Math.min(timeFromNext, currentActivity.maxTime)
    }

    def cond = minTimes.zip(maxTimes).forall({case (min, max) => min <= max})
    if (!cond) {
      println(activities.mkString("\n # "))
      println(activities.sliding(2).map({ case IndexedSeq(a,b) =>
        (a, b, distCost(a.location, b.location), startTimeDifference(a,b)) }).mkString("\n ? "))

      println(minTimes.toList)
      println(maxTimes.toList)
    }
    require(cond)

    (minTimes, maxTimes)
  }

  val activitiesWithTimes = (activities, minPossibleTimes, maxPossibleTimes).zipped.toIndexedSeq

  // Prepare a mapping from
  // request -> stops before dropoff
  // request -> stops after pickup
  // request -> current pickup stop index
  // request -> current dropoff stop index
  // request -> pickup location, dropoff location

  // stop index -> stop
  class FulfillmentInfo(
                        val stopsWithIndices: Array[BusStop],
                        val pickupStopIndex: Int,
                        val dropoffStopIndex: Int
                        ) {
    def stopsBeforeDropoff = stopsWithIndices.view(0, dropoffStopIndex)
    def stopsAfterPickup = stopsWithIndices.view(pickupStopIndex + 1, stopsWithIndices.length)
    def stopIndicesBeforeDropoff = 0 until dropoffStopIndex
    def stopIndicesAfterPickup = pickupStopIndex + 1 until stopsWithIndices.length
    def pickupLocation = stopsWithIndices(pickupStopIndex)
    def dropoffLocation = stopsWithIndices(dropoffStopIndex)
  }

  lazy val (requestsInfo, stopActivities, stopsWithIndices) = {
    val stopActivities = groupSuccessive(activities.filter(x => x.location.nonEmpty))(_.location.orNull)
    val byLocation = stopActivities.zipWithIndex

    val stopsWithIndices = byLocation.map({
      case ((location, activities), stopIndex) => location
    }).toArray

    val activityWithPickupIndex = byLocation.flatMap({
      case ((location, activities), stopIndex) => activities.flatMap({
        case Pickup(request, _) => Some((request, (location, stopIndex)))
        case _ => None
      })
    }).toMap
    val activityWithDropoffIndex = byLocation.flatMap({
      case ((location, activities), stopIndex) => activities.flatMap({
        case Dropoff(request, _) => Some((request, (location, stopIndex)))
        case _ => None
      })
    }).toMap

    val requests = activityWithDropoffIndex.keys
    val requestsWithFulfillmentInfo = requests.map(r => {
      (r, new FulfillmentInfo(stopsWithIndices, activityWithPickupIndex(r)._2, activityWithDropoffIndex(r)._2))
    }).toMap

    (requestsWithFulfillmentInfo, stopActivities, stopsWithIndices)
  }


  //////////////////
  ///////// METHODS

  def insertionCost(activity1 : (Activity, Double, Double),
                    activities: Seq[Activity],
                    activity2: (Activity, Double, Double)) : Double = {
    val (a1, m1, n1) = activity1
    val (a2, m2, n2) = activity2

    val activities2 = a1 +: activities :+ a2

    activities2.sliding(2).map({
      case Seq(a, b) => startTimeDifference(a, b)
    }).sum - startTimeDifference(a1, a2)
  }

  def _isInsertionFeasible(
                            startTime: Double,
                            a1: Activity,
                            a2: Activity,
                            ip1 : (Activity, Activity),
                            ip2: (Activity, Activity),
                            endTime: Double
                          ) = {

    val activityList = _insertedSubsequence(a1, a2, ip1, ip2)

    // Ensure each intermediate activity is feasible
    // M_2 <= m1 + t(a, 2) <= N_2
    // M_3 <= m1 + t(a, 2) + t(a,3) <= N_3
    // ...
    // M_y <= m1 + t(a, 2) + t(2, 3) + ... + t(?, Y) <= N_y
    val activityFeasibility = activityList.sliding(2).scanLeft(
      /* sum so far */
      (true, startTime) /* first activity's known min time */
    )({
      case ((_, timeSoFar), Seq(a, b)) =>
        val nextTime = timeSoFar + startTimeDifference(a, b)
        val t = Math.max(b.minTime, nextTime)

        (b.minTime <= t && t <= b.maxTime, t)
    }).drop(1).toList

    // All must be feasible
    // m_b2 <= m1 + ... + t(y, b2) <= n_b2
    activityFeasibility.forall(_._1) &&
      activityFeasibility.last._2 <= endTime /* last activity's known max time */
  }

  // Returns the cost, (insertion point 1), (insertion point 2) that would have been added
  def jobTryInsertion(request: Request)
        : Option[(Double, Activity, Activity, (Activity, Activity), (Activity, Activity))] = {
    if (request.time != time) None
    else {
      val pickupActivities = request.startStops.map(stop => new Pickup(request, stop))
      val dropoffActivities = request.endStops.map(stop => new Dropoff(request, stop))

      // The cost of inserting ... -> a -> x -> y -> b -> ...
      val bestDirectInsertWithCost = activitiesWithTimes.sliding(2).map({
        case Seq((a1, m1, n1), (a2, m2, n2)) =>
          val pds = for (p <- pickupActivities; d <- dropoffActivities) yield (p,d)
          val pdCosts = pds.map({
            case (p,d) =>
              if (_isInsertionFeasible(m1, p, d, (a1, a2), (a1, a2), n2))
                insertionCost((a1, m1, n1), Seq(p,d), (a2, m2, n2))
              else
                Double.PositiveInfinity
          })

          /* Best over all possible (p,d) */
          val ((bestP, bestD), bestCost) = (pds, pdCosts).zipped.minBy(_._2)
          ((bestP, bestD, (a1, a2), (a1, a2)), bestCost)
      })  /* Best over all possible insertion points */
          .minBy(_._2)

      val bestIndirectInsertWithCost = {
        // TODO: If we were writing a more general routing algorithm, we need to check
        // every combination of (a1, a2), (b1, b2) for feasibility
        // but this would give us quadratic explosion in cost. Since we know that all
        // requests have the same time window, we take the minimum of both and check their feasibility

        val pickupCosts = activitiesWithTimes.sliding(2).map({
          case Seq((a1, m1, n1), (a2, m2, n2)) =>
            val pickupCosts = pickupActivities.map(p => insertionCost((a1, m1, n1), Seq(p), (a2, m2, n2)))
            val best = (pickupActivities, pickupCosts).zipped.minBy(_._2)
            (best._2, best._1, (a1, m1, n1), (a2, m2, n2))
        })

        val dropoffCosts = activitiesWithTimes.sliding(2).drop(1).map({
          case Seq((b1, m1, n1), (b2, m2, n2)) =>
            val dropoffCosts = dropoffActivities.map(p => insertionCost((b1, m1, n1), Seq(p), (b2, m2, n2)))
            val best = (dropoffActivities, dropoffCosts).zipped.minBy(_._2)
            (best._2, best._1, (b1, m1, n1), (b2, m2, n2))
        })

        val minSubsequentDropoffCosts = dropoffCosts.foldRight(
          List[(Double, Activity, (Activity, Double, Double), (Activity, Double, Double))]()
        ) {
          case (s, Nil) => List(s)
          case (s, head :: tail) =>
            if (s._1 < head._1) s :: head :: tail
            else head :: head :: tail
        }

        val minPD = pickupCosts.toSeq.zip(minSubsequentDropoffCosts).minBy({
          case (p1, p2) => p1._1 + p2._1
        })

        val (timeCost, insertion) = minPD match {
          case ((pc, pa, pa1, pa2), (dc, da, da1, da2)) => (pc + dc, (pa, da, (pa1._1, pa2._1), (da1._1, da2._1)))
        }

        val feasibility = _isInsertionFeasible(
          minPD._1._3._2,
          insertion._1,
          insertion._2,
          insertion._3, insertion._4,
          minPD._2._4._3)

        val cost = if (feasibility) timeCost else Double.PositiveInfinity

        (insertion, cost)
      }

      Seq(bestDirectInsertWithCost, bestIndirectInsertWithCost)
        .minBy(_._2) match {
        case (x, Double.PositiveInfinity) => None
        case (x, finiteValue) => {
          // require(insertable(x))
          x match {
            case (b,c,(d,e),(f,g)) => {
//              try {
//                insert(b, c, (d, e), (f, g))
//              }
//              catch {
//                case ex : Exception => {
//                  val dMinTime = activitiesWithTimes.find(_._1 == d).orNull._2
//                  val gMaxTime = activitiesWithTimes.find(_._1 == g).orNull._3
//
//                  println("EXCEPTION ")
//
//                  println((dMinTime, gMaxTime))
//
//                  println(_insertedSubsequence(b, c, (d,e), (f,g)))
//                  println(_isInsertionFeasible(dMinTime, b,c,(d,e),(f,g), gMaxTime))
//
//                  println((b, c))
//                  println((d, e))
//                  println((f, g))
//
//                  throw ex
//                }
//              }

              Some(finiteValue, b,c,(d,e),(f,g))
            }
          }
        }
      }
    }
  }


  // For insertion x,y,(a1, a2),(b1,b2) returns a1 -> x -> a2 -> ... -> b1 -> b2

  def _insertedSubsequence(a1: Activity,
                            a2: Activity,
                            ip1 : (Activity, Activity),
                            ip2: (Activity, Activity)) = {
    if (ip1 == ip2) {
      IndexedSeq(ip1._1, a1, a2, ip1._2)
    }
    else {
      val indexOfFirst = activities.indexOf(ip1._1)
      val indexOfLast  = activities.indexOf(ip2._2)

      require(activities(indexOfFirst + 1) == ip1._2)
      require(activities(indexOfLast - 1) == ip2._1)

      IndexedSeq(ip1._1, a1) ++
      activities.slice(indexOfFirst + 1, indexOfLast) ++
      IndexedSeq(a2, ip2._2)
    }
  }

  def _insert(a1: Activity,
              a2: Activity,
              ip1 : (Activity, Activity),
              ip2: (Activity, Activity)) = {

    def tryInsert(a: Activity, acc: List[Activity]) = acc match {
      case b :: tail =>
        if ((a, b) == ip1 && (a, b) == ip2)
          a :: a1 :: a2 :: b :: tail
        else if ((a, b) == ip1)
          a :: a1 :: b :: tail
        else if ((a, b) == ip2)
          a :: a2 :: b :: tail
        else
          a :: b :: tail
      case Nil => a :: Nil
    }
    activities.foldRight(List[Activity]())(tryInsert)
  }

  def insert(a1: Activity, a2: Activity, ip1: (Activity, Activity), ip2: (Activity, Activity)) =
    new Route(
      routingProblem,
      _insert(a1, a2, ip1, ip2),
      time)

  /**
    * Make slight adjustments to each stop, insofar as routing time can be improved.
    *
    * @return improved route
    */
  @tailrec
  final def tweak : Route = {
    improve match {
      case None => this
      case Some(betterRoute) => betterRoute.tweak
    }
  }

  def improve : Option[Route] = {
    // activities --> [ (loc, [act1, act2, act3]), ... ]
    val locActivities = stopActivities

    /* Returns an iterator of Seq(prevStop, stop, nextStop */
    def locActivitiesWithSurrounding : Iterator[Seq[(Option[BusStop], Traversable[Activity])]] = {
      val mainIterator = locActivities.iterator.map({case (loc, as) => (Some(loc), as)})

      (Seq((None, List())).iterator ++ mainIterator ++ Seq((None, List())).iterator).sliding(3)
    }

    lazy val deletableStops = locActivitiesWithSurrounding.zipWithIndex
      /* Find the requests with alternative stops */
      /* Map to (do all activities at this location have an alternative?, stop index, cost savings) */
      .map({
      case (Seq((previousStop, _), (Some(loc), as), (nextStop, _)), stopIndex) =>
        // Does every activity have an alternative stop?
        val allFeasible = as.forall({
          case Pickup(request, _) =>
            (requestsInfo(request).stopsBeforeDropoff.toSet - loc intersect
              request.startStopsSet).nonEmpty
          case Dropoff(request, _) =>
            (requestsInfo(request).stopsAfterPickup.toSet - loc intersect
              request.endStopsSet).nonEmpty
        })

        // Compute savings...
        val savings = distCost(previousStop, Some(loc)) + distCost(nextStop, Some(loc)) -
          distCost(previousStop, nextStop)

        (allFeasible, stopIndex, savings)
    })
      .filter(_._1)

    lazy val replaceableStop = locActivitiesWithSurrounding.zipWithIndex
      /* map to (stopIndex, stop, cost delta) cost delta = change in cost if stop at stopIndex is replaced with stop */
      .flatMap({
      case (Seq((previousStop, _), (Some(loc), as), (nextStop, _)), stopIndex) =>
        val pickupSet : Set[BusStop] = as.map({
          case Pickup(request, _) => request.startStopsSet
          case Dropoff(request, _) => request.endStopsSet
        }).reduce( (s1, s2) => s1 intersect s2 )

        //
        val currentCost = distCost(previousStop, Some(loc)) + distCost(Some(loc), nextStop)

        // A list of (stopIndex, stop, cost delta). Cost delta < 0
        val improvers = (pickupSet - loc)
          .map(stop => (stopIndex, stop, distCost(previousStop, Some(stop)) + distCost(Some(stop), nextStop) - currentCost))
          .filter(_._3 < 0.0)

        if (improvers.isEmpty)
          None
        else {
          Some(improvers.minBy(_._3))
        }
    })

    if (deletableStops.nonEmpty) {
      //          println("Stop deleted")
      Some(withStopDeleted(deletableStops.maxBy(_._3 /* savings */)._2))
    }
    else if (replaceableStop.nonEmpty) {
//      println("Stop replaced")
      replaceableStop.minBy(_._3 /* cost delta */) match {
        case (stopIndex, replStop, costDelta) => Some(withStopChanged(stopIndex, replStop))
      }
    }
    else
      None
  }

  def withStopDeleted(stopIndex : Int) : Route = {
    /* Produce a sequence of (stopIndex, activity to update) */
    val updates = stopActivities(stopIndex)._2.map({
      case Pickup(request, location) =>
        val stopsInRoute = requestsInfo(request).stopIndicesBeforeDropoff.map(i => (stopsWithIndices(i), i)).toMap
        val suitableStops = request.startStops.filter(stop => stopsInRoute.contains(stop) && stop != location)

        require(suitableStops.nonEmpty)

        val replacement = suitableStops.head

        (stopsInRoute(replacement), Pickup(request, replacement))
      case Dropoff(request, location) =>
        val stopsInRoute = requestsInfo(request).stopIndicesAfterPickup.map(i => (stopsWithIndices(i), i)).toMap
        val suitableStops = request.endStops.filter(stop => stopsInRoute.contains(stop) && stop != location)

        require(suitableStops.nonEmpty)

        val replacement = suitableStops.head

        (stopsInRoute(replacement), Dropoff(request, replacement))
    })
      .groupBy(_._1)

    val newActivities = stopActivities.toIndexedSeq.zipWithIndex.flatMap({
      case ((stop, p_stopActivities), index) =>
        if (index == stopIndex)
          Seq()
        else {
          if (updates.contains(index))
            updates(index).toIterator.map(_._2) ++ p_stopActivities.toIterator
          else
            p_stopActivities
        }
    })

    val newRoute = new Route(routingProblem, StartActivity() +: newActivities :+ EndActivity(), time)

    require(newRoute.stopsWithIndices.length < this.stopsWithIndices.length)
    require(newRoute.activities.length == this.activities.length)

    newRoute
  }

  def withStopChanged(stopIndex : Int, newStop : BusStop) = {
    val newActivities = stopActivities.toIndexedSeq.zipWithIndex.flatMap({
      case ((stop, p_stopActivities), index) =>
        if (index == stopIndex) {
          p_stopActivities.map({
            case Pickup(request, _) => Pickup(request, newStop)
            case Dropoff(request, _) => Dropoff(request, newStop)
          })
        }
        else
          p_stopActivities
    })

    try {
      val newRoute = new Route(routingProblem, StartActivity() +: newActivities :+ EndActivity(), time)

      //      require(newRoute.stopsWithIndices.length == this.stopsWithIndices.length)
      require(newRoute.activities.length == this.activities.length)

      newRoute
    } catch {
      case err : IllegalArgumentException =>
        println(minPossibleTimes.toList)
        println(maxPossibleTimes.toList)
        println(stopActivities(stopIndex))
        println(newStop)
        println(activities.mkString("\n # "))

        throw err
    }

  }

  override def toString = {
    activities.mkString("\n> ")
  }
}

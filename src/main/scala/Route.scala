package sg.beeline

object Route {
  def empty(routingProblem: RoutingProblem, time: Double) =
    new Route(routingProblem, List(new StartActivity, new EndActivity), time)

}

class Route(val routingProblem: RoutingProblem,
            _activities : Seq[Activity],
            val time: Double) {

  val activities = _activities.toArray

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

    for (i <- 1 to (len - 1)) {
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
      println(minTimes.toList)
      println(maxTimes.toList)
    }
    require(cond)

    (minTimes, maxTimes)
  }

  val activitiesWithTimes = (activities, minPossibleTimes, maxPossibleTimes).zipped

  // Cost between two points. Accepts options. Returns 0 if either is None
  def distCost(optLoc1: Option[BusStop], optLoc2: Option[BusStop]) =
    ((optLoc1, optLoc2) match {
      /* For every 60 kilometers, I take 3600 seconds */
      case (Some(l1), Some(l2)) => routingProblem.distance(l1, l2) / 60.0 * 3600.0
      case _ => 0.0
    })

  def insertionCost(activity1 : (Activity, Double, Double),
                    activities: Seq[Activity],
                    activity2: (Activity, Double, Double)) : Double = {
    val (a1, m1, n1) = activity1
    val (a2, m2, n2) = activity2

    val activities2 = a1 +: activities :+ a2

    val minTimes = activities2.sliding(2).scanLeft(m1) {
      case (acc, Seq(lastActivity, currentActivity)) =>
        Math.max(acc + startTimeDifference(lastActivity, currentActivity), currentActivity.minTime)
    }
    val maxTimes = activities2.sliding(2).scanRight(n2) {
      case (Seq(currentActivity, nextActivity), acc) =>
        Math.min(currentActivity.maxTime, acc - startTimeDifference(currentActivity, nextActivity) )
    }

    val isFeasible = (minTimes zip maxTimes).forall(x => x._1 <= x._2)

    if (!isFeasible) Double.PositiveInfinity
    else
      activities2.sliding(2).map({case Seq(a, b) => distCost(a.location, b.location)}).sum -
      distCost(a1.location, a2.location)
  }

  // Returns the cost, (insertion point 1), (insertion point 2) that would have been added
  def jobTryInsertion(request: Request)
        : Option[(Double, Activity, Activity, (Activity, Activity), (Activity, Activity))]= {
    if (request.time != time) None
    else {
      val pickupActivities = request.startStops.map(stop => new Pickup(request, stop))
      val dropoffActivities = request.endStops.map(stop => new Dropoff(request, stop))

      def insertionPoints = activitiesWithTimes.toSeq.sliding(2)

      // Cost of inserting X -> Y directly between a preexisting pair
      // A -> B --> A -> X -> Y -> B
      val directTripCosts = insertionPoints.map(p => p match {
        case Seq((a1, m1, n1), (a2, m2, n2)) => (for (a <- pickupActivities; b <- dropoffActivities) yield (a,b))
          .map(p => p match {
            case (a,b) => (
              insertionCost((a1, m1, n1), List(a, b), (a2, m2, n2)),
              a, b,
              (a1, a2), (a1, a2)
            )
          }).minBy(_._1)
        })


      // dropoffCosts
      def dropoffCosts = insertionPoints.map(p => p match {
        // over all the possible pickup points
        case Seq((a1, m1, n1), (a2, m2, n2)) => pickupActivities.map(a => (
          insertionCost((a1, m1, n1), List(a), (a2, m2, n2)),
          a,
          (a1,a2)
        )).minBy(_._1)
      })
      def subsequentPickupCosts = insertionPoints.drop(1).map(ip => ip match {
        // over all the possible dropoff points
        case Seq((b1, m1, n1), (b2, m2, n2)) => dropoffActivities.map(b => (
          insertionCost((b1, m1, n1), List(b), (b2, m2, n2)),
          b,
          (b1,b2)
        )).minBy(_._1)
      })
      .foldRight[List[(Double, Activity, (Activity, Activity))]
        ](List())((s, t) => (s, t) match {
        case (s, Nil) => List(s)
        case ((cost1, b1, ip1), (cost2, b2, ip2) :: tail) =>
          if (cost1 < cost2) (cost1, b1, ip1) :: tail
          else (cost2, b2, ip2) :: tail
      })

      // Cost of ... -> A -> X -> B -> ... -> Y -> ...
      val indirectTripCosts = dropoffCosts.zip(subsequentPickupCosts.toIterator)
        .map(p => p match {
          case ((dCost, a, (a1, a2)), (pCost, b, (b1, b2))) =>
            (dCost + pCost, a, b, (a1, a2), (b1, b2))
        })

      val lowestCostInsertion = List(directTripCosts.minBy(_._1),
                                      indirectTripCosts.minBy(_._1)).minBy(_._1)

      if (lowestCostInsertion._1 != Double.PositiveInfinity)
        Some(lowestCostInsertion)
      else
        None
    }
  }

  def insert(a1: Activity,
              a2: Activity,
              ip1 : (Activity, Activity),
              ip2: (Activity, Activity)) = {

    def tryInsert(a : Activity, acc : List[Activity]) = acc match {
      case b :: tail =>
        if ((a,b) == ip1 && (a,b) == ip2)
          a :: a1 :: a2 :: b :: tail
        else if ((a,b) == ip1)
          a :: a1 :: b :: tail
        else if ((a,b) == ip2)
          a :: a2 :: b :: tail
        else
          a :: b :: tail
      case Nil => a :: Nil
    }

    new Route(
      routingProblem,
      activities.foldRight(List[Activity]())(tryInsert),
      time)
  }
}

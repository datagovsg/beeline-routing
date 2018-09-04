package sg.beeline.problem

import sg.beeline.util.{ListInsertionPointOps, ListOps}

import scala.annotation.tailrec
import scala.reflect.ClassTag

// The main objective of Route2 is to optimize a different problem:
// Ensure that each suggestion does not encounter a more-than-necessary
// detour.
// The assumptions are also simplified:
// - a list of pickup stops
// - a list of dropoff stops
// - pickup stops will always come before the dropoff stops
// - time of request is irrelevant
class Route2(val routingProblem: RoutingProblem,
             val detourBudget: Double)
            (val pickups: IndexedSeq[(BusStop, List[Request])],
             val dropoffs: IndexedSeq[(BusStop, List[Request])])
{
  def travelCosts(s: Iterable[BusStop]) = {
    s.sliding(2).map { case Seq(a, b) =>
      require(a != b)
      routingProblem.distance(a, b) + 60000
    }.sum
  }

  /**
    * FIXME: reduce the number of special cases??
    * @param request
    * @return
    */
  def tryInsertingStops(request: Request): Option[Route2] = {
    val matchingPickupIndex = pickups.indexWhere(request.startStopsSet contains _._1)
    val matchingDropoffIndex = dropoffs.indexWhere(request.endStopsSet contains _._1)

    // Because Array.concat only accepts a sequence of arrays? WTF?
    def arrayConcat[X: ClassTag](xss: Seq[X]*) = {
      val builder = Array.newBuilder[X]
      xss.foreach(builder ++= _)
      builder.result
    }

    val updatedPickups =
      if (matchingPickupIndex != -1) {
        val (pickupStop, pickupRequests) = pickups(matchingPickupIndex)
        Some(pickups.updated(matchingPickupIndex, pickupStop -> (request :: pickupRequests)))
      } else {
        None
      }
    val updatedDropoffs =
      if (matchingDropoffIndex != -1) {
        val (dropoffStop, dropoffRequests) = dropoffs(matchingDropoffIndex)
        Some(dropoffs.updated(matchingDropoffIndex, dropoffStop -> (request :: dropoffRequests)))
      } else {
        None
      }

    val currentBaseCost = travelCosts(List(pickups.head._1, dropoffs.last._1))
    val currentTravelCost = travelCosts(pickups.view.map(_._1) ++ dropoffs.view.map(_._1))

    // For some strange reason, this assertion can sometimes fail
    // Must be a bug in graphhopper shortest path search?
//    if (currentTravelCost < currentBaseCost) {
//      println(pickups.map(_._1) ++ dropoffs.map(_._1))
//      println((currentTravelCost - currentBaseCost) / 3600e3)
//      require(false)
//    }

    val currentDetour = currentTravelCost - currentBaseCost
    val remainingBudget = detourBudget - currentDetour

    type SingleSet = Array[(BusStop, List[Request])]

    lazy val insertPickupCosts: IndexedSeq[(Double, () => SingleSet)] = for {
        insertBefore <- pickups.indices.drop(1)
        start <- request.startStops
        sunkCost = travelCosts(Array(pickups(insertBefore - 1)._1, pickups(insertBefore)._1))
        newCost = travelCosts(Array(pickups(insertBefore - 1)._1, start, pickups(insertBefore)._1))
      } yield (
        newCost - sunkCost,
        () => arrayConcat(
          pickups.slice(0, insertBefore),
          Array((start, List(request))),
          pickups.drop(insertBefore)
        )
      )

    lazy val insertDropoffCosts: IndexedSeq[(Double, () => SingleSet)] = for {
      insertBefore <- dropoffs.indices.drop(1)
      end <- request.endStops
      sunkCost = travelCosts(Array(dropoffs(insertBefore - 1)._1, dropoffs(insertBefore)._1))
      newCost = travelCosts(Array(dropoffs(insertBefore - 1)._1, end, dropoffs(insertBefore)._1))
    } yield (
      newCost - sunkCost,
      () => arrayConcat(
        dropoffs.slice(0, insertBefore),
        Array((end, List(request))),
        dropoffs.drop(insertBefore)
      )
    )

    implicit class ForSafeMinBy[A, M[A] <: TraversableOnce[A]](m: M[A]) {
      def safeMinBy[B: Ordering](f: A => B): Option[A] = {
        if (m.isEmpty) None
        else Some(m.minBy[B](f))
      }
    }
    implicit class ForSafeMinByArray[A](m: Array[A]) {
      def safeMinBy[B: Ordering](f: A => B): Option[A] = {
        if (m.isEmpty) None
        else Some(m.minBy[B](f))
      }
    }

    // SPECIAL CASES:
    // 1. Insert before all stops
    // 2. Insert after all stops
    // Need to account for each one separately
    // P1 x D1
    // P2 x D1
    // P1 x D2
    // P2 x D2
    lazy val specialCases = Array[IndexedSeq[(Double, () => (SingleSet, SingleSet))]](
      // Before first pickup, before first dropoff
      {
        for {
          start <- request.startStops
          end <- request.endStops
          detour = currentTravelCost + // A1_A2 + A2_A3 + A3_B1 + B1_B2
            travelCosts(Array(start, pickups.head._1)) + // + A0_A1
            travelCosts(Array(pickups.last._1, end, dropoffs.head._1)) - // + A3_B0 + B0_B1
            travelCosts(Array(pickups.last._1, dropoffs.head._1)) - // - A3_B1
            travelCosts(Array(start, dropoffs.last._1)) // - A0-B2
        } yield (
          detour,
          () => (
            Array((start, List(request))) ++ pickups,
            Array((end, List(request))) ++ dropoffs
          )
        )
      },

      // Before first pickup, after last dropoff,
      {
        for {
          start <- request.startStops
          end <- request.endStops
          detour = currentTravelCost + // A1_A2 + A2_A3 + A3_B1 + B1_B2
            travelCosts(Array(start, pickups.head._1)) + // + A0_A1
            travelCosts(Array(dropoffs.last._1, end)) - // + B2_B3
            travelCosts(Array(start, end)) // - A0_B3
        } yield (
          detour,
          () => (
            arrayConcat(Array((start, List(request))), pickups),
            arrayConcat(dropoffs, Array((end, List(request))))
          )
        )
      },

      // After last pickup, before first dropoff
      {
        for {
          start <- request.startStops
          end <- request.endStops
          detour = currentDetour +
            travelCosts(Array(pickups.last._1, start, end, dropoffs.head._1)) -
            travelCosts(Array(pickups.last._1, dropoffs.head._1))
        } yield (
          detour,
          () => (
            arrayConcat(pickups, Array((start, List(request)))),
            arrayConcat(Array((end, List(request))), dropoffs)
          )
        )
      },

      // After last pickup, after last dropoff
      {
        for {
          start <- request.startStops
          end <- request.endStops
          detour = currentTravelCost + // A1_A2 + A2_A3 + A3_B1 + B1_B2
            travelCosts(Array(pickups.last._1, start, dropoffs.head._1)) - // + A3_A4 + A4_B1
            travelCosts(Array(pickups.last._1, dropoffs.head._1)) + // - A3_B1
            travelCosts(Array(dropoffs.last._1, end)) - // + B2_B3
            travelCosts(Array(pickups.head._1, end)) // - A0_B3
        } yield (
          detour,
          () => (
            arrayConcat(pickups, Array((start, List(request)))),
            arrayConcat(dropoffs, Array((end, List(request))))
          )
        )
      }
    ).flatten.safeMinBy(_._1)

    if (updatedPickups.nonEmpty && updatedDropoffs.nonEmpty) {
      Some(new Route2(routingProblem, detourBudget)(updatedPickups.get, updatedDropoffs.get))
    } else if (updatedPickups.isEmpty && updatedDropoffs.nonEmpty) {
      // 2x special cases -- at the start, at the end
      val pickupSpecialCases: Array[(Double, () => SingleSet)] = Array(
        { // before first pickup stop
          for {
            start <- request.startStops
            detour = currentTravelCost +
              travelCosts(Array(start, pickups.head._1)) -
              travelCosts(Array(start, dropoffs.last._1))
          } yield (
            detour,
            () => arrayConcat(List((start, List(request))), pickups)
          )
        },
        { // after last pickup stop
          for {
            start <- request.startStops
            detour = currentDetour +
              travelCosts(Array(pickups.last._1, start, dropoffs.head._1)) -
              travelCosts(Array(pickups.last._1, dropoffs.head._1))
          } yield (
            currentDetour + detour,
            () => arrayConcat(pickups, List((start, List(request))))
          )
        }
      ).flatten

      val combined: Option[(Double, () => SingleSet)] =
        (pickupSpecialCases.iterator ++
        insertPickupCosts.iterator.map { case (change, f) => (currentDetour + change, f) })
        .safeMinBy(_._1)

      (pickupSpecialCases.iterator ++ insertPickupCosts.map { case (change, f) => (currentDetour + change, f) })
        .safeMinBy(_._1)
        .filter(_._1 <= detourBudget)
        .map { case (_, fn) =>
            new Route2(routingProblem, detourBudget)(fn(), updatedDropoffs.get)
        }
    } else if (updatedPickups.nonEmpty && updatedDropoffs.isEmpty) {
      val dropoffSpecialCases = Array(
        { // before first dropoff stop
          for {
            end <- request.endStops
            detour = currentDetour +
              travelCosts(Array(pickups.last._1, end, dropoffs.head._1)) -
              travelCosts(Array(pickups.last._1, dropoffs.head._1))
          } yield (
            detour,
            () => arrayConcat(Array((end, List(request))), dropoffs)
          )
        },
        { // after last pickup stop
          for {
            end <- request.endStops
            detour = currentTravelCost +
              travelCosts(Array(dropoffs.last._1, end)) -
              travelCosts(Array(pickups.head._1, end))
          } yield (
            currentDetour + detour,
            () => arrayConcat(dropoffs, Array((end, List(request))))
          )
        }
      ).flatten

      (dropoffSpecialCases.iterator ++ insertDropoffCosts.map { case (change, f) => (currentDetour + change, f) })
        .safeMinBy(_._1)
        .filter(_._1 <= detourBudget)
        .map { case (_, fn) =>
          new Route2(routingProblem, detourBudget)(updatedPickups.get, fn())
        }
    } else { // if (updatedPickups.isEmpty && updatedDropoffs.isEmpty) { // cannot use any existing stops
      val bestInsertCase = for {
        (pcost, bestPickupInsert) <- insertPickupCosts.reduceOption { (a, b) => List(a, b).minBy(_._1) }
        (dcost, bestDropoffInsert) <- insertDropoffCosts.reduceOption { (a, b) => List(a, b).minBy(_._1) }
        if pcost + dcost <= remainingBudget
      } yield {
        (currentDetour + pcost + dcost) ->
          (() => (bestPickupInsert(), bestDropoffInsert()))
      }
      val bestSpecialCase = specialCases
        .filter(_._1 <= detourBudget)

      List(bestInsertCase, bestSpecialCase)
        .flatten
        .reduceOption( (a, b) => List(a, b).minBy(_._1) )
        .map { case (_, fn) =>
            val (newPickups, newDropoffs) = fn()
            new Route2(routingProblem, detourBudget)(newPickups, newDropoffs)
        }
    }
  }

  // Returns the cost, (insertion point 1), (insertion point 2) that would have been added
  def jobTryInsertion(request: Request)
        : Option[Route2] = {
    val pickupStops = request.startStops
    val dropoffStops = request.endStops

    if (pickupStops.isEmpty || dropoffStops.isEmpty) None
    else {
      tryInsertingStops(request)
    }
  }

  /* Generate a list of all the stops in this route.
    Useful for deduplicating routes
   */
  lazy val stops = (pickups.iterator ++ dropoffs.iterator).map(_._1).toList
  lazy val requests = {
    require { pickups.flatMap(_._2).toSet == dropoffs.flatMap(_._2).toSet }
    (pickups.flatMap(_._2))
  }

  def times(endTime: Double) = {
    (pickups.view.map(_._1) ++ dropoffs.view.map(_._1))
      .sliding(2)
      .foldRight( List(endTime) ) { case (Seq(a, b), time) =>
        (time.head - travelCosts(Seq(a, b))) :: time
      }
      .toList
  }

  override def equals(other : Any) : Boolean = other match {
    case otherRoute : Route2 =>
      routingProblem == otherRoute.routingProblem &&
        detourBudget == otherRoute.detourBudget &&
        pickups.sameElements(otherRoute.pickups) &&
        dropoffs.sameElements(otherRoute.dropoffs)
    case _ => false
  }

  override def hashCode =
    routingProblem.hashCode +
      detourBudget.hashCode +
      pickups.hashCode +
      dropoffs.hashCode

  override def toString =
    (pickups ++ dropoffs)
      .mkString("\n> ")
}

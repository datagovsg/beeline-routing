package sg.beeline
import scala.annotation.tailrec
import scala.collection.immutable.HashSet
import scala.collection.immutable.Stream.Empty


trait Ruin {
  def ruin(problem :RoutingProblem, routes: Traversable[Route], badRequests: Traversable[Request])
    : (Traversable[Route], Traversable[Request])
}

object Ruin extends Ruin {
  val RUIN_PROBABILITY = 0.5


  def ruin(problem: RoutingProblem, routes: Traversable[Route], badRequests: Traversable[Request]) = {

    // Random Ruin
    val stopsToDestroy = if (Math.random() <= 0.2) {
      // Collect the set of all stops
      val allStops = routes.flatMap(r => r.activities.flatMap(_.location))
        .toSet

      println(s"${allStops.size} stops")

      allStops.filter((r) => Math.random() < RUIN_PROBABILITY)
    }

    //Worst Ruin
    else {
      //Calculate the cost for each of the stops
      val stopCosts = routes.map(r => {
        val midStopCost = r.activities.sliding(3).map({
          case Seq(previousActivity, currentActivity, nextActivity) =>
            r.startTimeDifference(previousActivity, currentActivity) +
              r.startTimeDifference(currentActivity, nextActivity) -
              r.startTimeDifference(previousActivity, nextActivity)
        }).toSeq.view

        List(0.0).view ++ midStopCost.view ++ List(0.0).view
      }).toList

      //Get the list for activities for each route
      val allRouteStops = routes.map(r => r.activities).toList

      //Zip the list of activities and costs
      val routeStopCost = (allRouteStops zip stopCosts).map {
        case (a, b) => (a zip b)
      }

      //Worst stop for each route
      routeStopCost.map(r => {
        r.maxBy(_._2)._1.location.orNull
      }).toSet
    }


    val (preservedRoutes, unservedRequests) = routes.foldLeft(
      (List[Route](), new HashSet[Request]())
    )((acc, route) => {
      val (routeList, unservedRequests) = acc

      val allUnservedRequests = unservedRequests ++ badRequests ++
        route.activities
            .flatMap({
              case Pickup(req, loc) => if (stopsToDestroy contains loc) Some(req) else None
              case Dropoff(req, loc) => if (stopsToDestroy contains loc) Some(req) else None
              case _ => None
            })

      val newActivities = route.activities.filter({
        case Pickup(req, loc) => !(allUnservedRequests contains req)
        case Dropoff(req, loc) => !(allUnservedRequests contains req)
        case _ => true
      })

      (new Route(route.routingProblem, newActivities, route.time) :: routeList, allUnservedRequests)
    })

    val rv = (preservedRoutes.filter(_.activities.size > 2), unservedRequests)

    println(s"Ruined ${routes.size - rv._1.size} of ${routes.size} routes, ${unservedRequests.size} requests")

    rv
  }
}

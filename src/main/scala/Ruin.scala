package sg.beeline
import scala.annotation.tailrec
import scala.collection.immutable.HashSet

object Ruin {
  val RUIN_PROBABILITY = 0.5

  // Random ruin
  def ruin(problem: RoutingProblem, routes: List[Route], badRequests: Seq[Request]) = {

    // Collect the set of all stops
    val allStops = routes.flatMap(r => r.activities.flatMap(_.location))
      .toSet

    println(s"${allStops.size} stops")

    val stopsToDestroy = allStops
      .filter((r) => Math.random() < RUIN_PROBABILITY)

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

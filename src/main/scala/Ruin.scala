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

  def randomRuin(problem: RoutingProblem, routes: Traversable[Route], badRequests: Traversable[Request],
                 ruinProbability: Double) = {
    // Collect the set of all stops
    val stopsToDestroy = {
      val allStops = routes.flatMap(r => r.activities.flatMap(_.location))
        .toSet

      println(s"${allStops.size} stops")

      allStops.filter((r) => Math.random() < ruinProbability)
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

  def worstRuin(problem: RoutingProblem, routes: Traversable[Route], badRequests: Traversable[Request],
                numToRuin : Int) = {
    @tailrec
    def ruinWorst(currentRoutes : Traversable[Route], ruinedRequests : List[Set[Request]], count : Int)
                : (Traversable[Route], Traversable[Request]) = {

      if (count == 0)
        (currentRoutes, ruinedRequests.flatten)
      else {
        def mostCostlyStop(route: Route): (Route, BusStop, Int, Double) = {
          route.locActivitiesWithSurrounding.zipWithIndex.map({
            case (Seq((previousStop, _), (Some(loc), as), (nextStop, _)), stopIndex) =>
              val cost =
                route.distCost(previousStop, Some(loc)) +
                  route.distCost(Some(loc), nextStop) -
                  route.distCost(previousStop, nextStop)

              (route, loc, stopIndex, cost)
          })
            .maxBy(_._4)
        }

        val costliestStops = currentRoutes.map(mostCostlyStop).maxBy(_._4)
        val costliestRoute = costliestStops._1

        // Discard this stop from the route
        val thisRuinedRequests: Set[Request] = costliestRoute.stopActivities(costliestStops._3)._2.map({
          case Pickup(request, _) => request
          case Dropoff(request, _) => request
        }).toSet

        val newRoute = new Route(
          costliestRoute.routingProblem,
          costliestRoute.activities.filter({
            case Pickup(request, _) => !thisRuinedRequests.contains(request)
            case Dropoff(request, _) => !thisRuinedRequests.contains(request)
            case _ => true
          }),
          costliestRoute.time
        )

        ruinWorst(
          if (newRoute.activities.length < 4)
            currentRoutes.filter(_ != costliestRoute)
          else
            currentRoutes.map(x => if (x == costliestRoute) newRoute else x),
          thisRuinedRequests :: ruinedRequests,
          count - 1
        )
      }
    }

    ruinWorst(routes, List(), numToRuin)
  }


  def ruin(problem: RoutingProblem, routes: Traversable[Route], badRequests: Traversable[Request]) = {
    // Random Ruim -- randomly destroy between 20% and 50% of stops
    if (Math.random() <= 0.2) {
      randomRuin(problem, routes, badRequests, 0.3 + Math.random() * 0.4)
    } else {
      worstRuin(problem, routes, badRequests, ((0.1 + (Math.random() * 0.3)) * routes.map(_.stopActivities.size).sum).toInt)
    }
  }
}

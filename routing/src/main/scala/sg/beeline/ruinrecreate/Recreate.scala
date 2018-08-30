package sg.beeline.ruinrecreate

import sg.beeline._
import sg.beeline.problem._

import scala.util.Random

trait Recreate {
  def recreate(problem : RoutingProblem, preservedRoutes : Traversable[Route], unservedRequests : Traversable[Request])
   : (Traversable[Route], Traversable[Request])
}

object Recreate extends Recreate {
  var count : Int = 0

  def iterRequest(problem : RoutingProblem)(acc : (List[Route], List[Request]), request: Request)
  : (List[Route], List[Request]) = {
    if (count % 100 == 0) {
      println(count)
    }
    count += 1

    val (routes, badRequests) = acc

    val routeCosts = routes.par
      .map(r => r.jobTryInsertion(request)(24 * 3600 * 1000))

    val routeCostsRoutes = routeCosts.zip(routes)
      .filter({case (None, _) => false case _ => true})

    if (routeCosts.forall({case None => true case _ => false})) {
      // Construct new route
      val randomPickup = new Pickup(request, request.startStops(Random.nextInt(request.startStops.size)))
      val randomDropoff = new Dropoff(request, request.endStops(Random.nextInt(request.endStops.size)))

      if (Route.distCost(problem)(randomPickup.location, randomDropoff.location) == Double.PositiveInfinity)
        (routes, request :: badRequests)
      else {
        val newRoutes = new Route(problem, List(new StartActivity, randomPickup, randomDropoff, new EndActivity), request.time) :: routes

        (newRoutes, badRequests)
      }
    }
    else {
      // Insert into min cost route
      val minCostRoute = routeCostsRoutes
        .minBy(_._1.orNull._1)

      val newRoutes = routes.map(r =>
        if (r == minCostRoute._2) {
          val (cost, a1, a2, ip1, ip2) = minCostRoute._1.orNull
          r.insert(a1, a2, ip1, ip2)
        }
        else r
      )
      (newRoutes, badRequests)
    }
  }

  def recreate(problem : RoutingProblem, preservedRoutes : Traversable[Route], unservedRequests : Traversable[Request]) =
    // ||-ize
    Random.shuffle(unservedRequests).foldLeft(
      (preservedRoutes.toList, List[Request]())
    )(iterRequest(problem))
}

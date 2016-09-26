package sg.beeline

import scala.util.Random

object Recreate {
  var count : Int = 0

  def iterRequest(problem : RoutingProblem)(routes : List[Route], request: Request) : List[Route] = {
    if (count % 100 == 0) {
      println(count)
    }
    count += 1

    val routeCosts = routes
      .map(r => r.jobTryInsertion(request))

    val routeCostsRoutes = routeCosts.zip(routes)
      .filter({case (None, _) => false case _ => true})

    if (routeCosts.forall({case None => true case _ => false})) {
      // Construct new route
      new Route(problem,
                List(new StartActivity,
                     new Pickup(request, request.startStops({Random.nextInt % request.startStops.size})),
                     new Dropoff(request, request.endStops({Random.nextInt % request.startStops.size})),
                     new EndActivity), request.time) :: routes
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
      newRoutes
    }
  }

  def recreate(problem : RoutingProblem, preservedRoutes : Seq[Route], unservedRequests : Traversable[Request]) =
    // ||-ize
    unservedRequests.foldLeft(preservedRoutes.toList)(iterRequest(problem))
}

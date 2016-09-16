package sg.beeline

object Hi {
  def main(args: Array[String]) {
    val problem = new BasicRoutingProblem(Import.getBusStops, Import.getRequests)

    def iterSolution(routes : List[Route], requests : List[Request])
      : (List[Route], Seq[Request]) = {
        // Ruin
        val (preservedRoutes, unservedRequests) = Ruin.ruin(problem, routes, requests)

        // Recreate
        val newRoutes = Recreate.recreate(problem, preservedRoutes, unservedRequests)

        (newRoutes, requests)
      }

    val (routes, requests) = problem.initialize
    val (newRoutes, newRequests, score) = (0 until 10)
      .foldLeft(
          (routes.toList, requests.toList, Double.NegativeInfinity)
        )((acc, iterCount) => {
        val (routeList, requestList, previousScore) = acc
        val (nextRouteList, nextRequests) = iterSolution(routeList, requestList)
        val nextScore = Score.score(nextRouteList)

        // FIXME: Use Threshold Annealing
        if (nextScore > previousScore) {
          println(s"Score decreased to ${nextScore}")
          (nextRouteList, requestList, nextScore)
        }
        else {
          println(s"Score maintained at ${previousScore}")
          (routeList, requestList, previousScore)
        }
      })

    SolutionPrinter.writeProblem(requests)
    SolutionPrinter.writeLocations(problem.busStops)
    SolutionPrinter.writeSolution(newRoutes)
  }
}

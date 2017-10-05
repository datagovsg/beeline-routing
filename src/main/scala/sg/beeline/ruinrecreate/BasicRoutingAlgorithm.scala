package sg.beeline.ruinrecreate

import sg.beeline.problem.{Request, RoutingProblem, Route}
import sg.beeline.ruinrecreate.{BeelineRecreate, Score, Recreate, Ruin}

class BasicRoutingAlgorithm(val problem : RoutingProblem)
                           (implicit val ruin : Ruin = Ruin,
                            implicit val recreate : Recreate = Recreate,
                            implicit val score : Score = Score)

  extends sg.beeline.ruinrecreate.RoutingAlgorithm
{

  @volatile var currentRoutes : Traversable[Route] = null

  def currentSolution = this.currentRoutes

  def run {
    println("Basic Routing Algorithm:")
    val (routes, requests, badRequests) = problem.initialize
    val beelineRecreate = new BeelineRecreate(problem, requests)

    println("Initialized")

    def iterSolution(routes : Traversable[Route], badRequests : Traversable[Request])
    : (Traversable[Route], Traversable[Request]) = {
      // Ruin
      val (preservedRoutes, unservedRequests) = Ruin.ruin(problem, routes, badRequests)

      println("Ruined")

      // Recreate
      val (newRoutes, newBadRequests) = beelineRecreate.recreate(problem, preservedRoutes, unservedRequests)

      //      val (newRoutes, newBadRequests) = LowestRegretRecreate.recreate(problem, preservedRoutes, unservedRequests)
      println("Recreated")

      (newRoutes, newBadRequests)
    }

    currentRoutes = routes

    val numIter = 100
    val (newRoutes, newBadRequests, score) = (0 until numIter)
      .foldLeft(
        (routes, badRequests, Double.NegativeInfinity)
      )((acc, iterCount) => {
        if (shouldStop) {
          (null, null, 0)
        }
        else {
          val (routeList, requestList, previousScore) = acc
          val (nextRouteList, nextRequests) = iterSolution(routeList, badRequests)
          val nextScore = Score.score(nextRouteList)

          val threshold = {
            val T_0 = 500

            T_0 * math.exp(- iterCount / numIter.toDouble / 0.1)
          }

          // FIXME: Use Threshold Annealing
          if (nextScore > previousScore) {
            println(s"Score increased to ${nextScore}")

            // Make the routes available for web access
            currentRoutes = nextRouteList

            (nextRouteList, requestList, nextScore)
          }
          else if (nextScore > previousScore - threshold) {
            println(s"Temporarily accepting score at ${nextScore}")

            // Temporarily accept the new routes, but
            // don't accept it as the best...
            currentRoutes = nextRouteList

            (nextRouteList, requestList, nextScore)
          }
          else {
            println(s"Score maintained at ${previousScore}")
            (routeList, requestList, previousScore)
          }
        }
      })
  }

}

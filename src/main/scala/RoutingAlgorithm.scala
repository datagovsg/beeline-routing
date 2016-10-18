package sg.beeline

import akka.actor.{ActorContext, ActorRef}
import sg.beeline.ui.{RoutingStopped, CurrentSolution, StopRouting}

trait RoutingAlgorithm extends Runnable {
  @volatile var shouldStop = false

  def currentSolution: Traversable[Route]
  def run : Unit

  def solve(context: ActorContext) : PartialFunction[Any, Unit] = {
    val thread = new Thread(this)

    thread.start()

    {
      case StopRouting => {
        this.shouldStop = true

        if (!thread.isAlive) {
          thread.join(60000)
        }
        context.sender ! RoutingStopped
        context.unbecome()
      }
      case CurrentSolution => context.sender ! currentSolution
    }
  }
}

class BasicRoutingAlgorithm(val problem : RoutingProblem)
                           (implicit val ruin : Ruin = Ruin,
    implicit val recreate : Recreate = Recreate,
    implicit val score : Score = Score)

extends RoutingAlgorithm
{

  @volatile var currentRoutes : Traversable[Route] = null

  def currentSolution = this.currentRoutes

  def run {
    println("Basic Routing Algorithm:")
    val (routes, requests, badRequests) = problem.initialize
//    val beelineRecreate = new BeelineRecreate(problem, requests)

    println("Initialized")

    def iterSolution(routes : Traversable[Route], badRequests : Traversable[Request])
    : (Traversable[Route], Traversable[Request]) = {
      // Ruin
      val (preservedRoutes, unservedRequests) = Ruin.ruin(problem, routes, badRequests)

      println("Ruined")

      // Recreate
//      val (newRoutes, newBadRequests) = beelineRecreate.recreate(problem, preservedRoutes, unservedRequests)


      val (newRoutes, newBadRequests) = LowestRegretRecreate.recreate(problem, preservedRoutes, unservedRequests)
      println("Recreated")

      (newRoutes, newBadRequests)
    }

    currentRoutes = routes

    val (newRoutes, newBadRequests, score) = (0 until 100)
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

          // FIXME: Use Threshold Annealing
          if (nextScore > previousScore) {
            println(s"Score increased to ${nextScore}")

            // Make the routes available for web access
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

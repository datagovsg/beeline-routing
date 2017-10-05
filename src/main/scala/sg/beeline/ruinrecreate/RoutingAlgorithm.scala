package sg.beeline.ruinrecreate

import akka.actor.ActorContext
import sg.beeline.jobs.{RoutingStopped, CurrentSolution, StopRouting}
import sg.beeline.problem.Route

trait RoutingAlgorithm extends Runnable {
  @volatile var shouldStop = false

  def currentSolution: Traversable[Route]
  def run : Unit


  // FIXME: The callback thing is an ugly hack.
  // We should be setting up an Actor system / use futures etc
  def solve(context: ActorContext, callback : Traversable[Route] => Any) : PartialFunction[Any, Unit] = {
    val thread = new Thread(this)

    thread.start()

    {
      case StopRouting => {
        this.shouldStop = true

        if (!thread.isAlive) {
          thread.join(60000)
        }
        context.sender ! RoutingStopped
        callback(currentSolution)
        context.unbecome()
      }
      case CurrentSolution =>
        context.sender ! currentSolution
        println("Current solution sent")
    }
  }
}

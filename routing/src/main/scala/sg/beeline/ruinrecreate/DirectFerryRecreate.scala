package sg.beeline.ruinrecreate

import sg.beeline._
import sg.beeline.problem.{Route, Request, RoutingProblem}

/**
  * A fake recreate algorithm -- it just tries to create a direct trip for every request
  *
  * Useful for initialization though
  */
object DirectFerryRecreate extends Recreate {

  def recreate(problem : RoutingProblem, preservedRoutes : Traversable[Route], requests : Traversable[Request]) = {

    val (routes, badRequests) = requests.foldLeft(
      (List[Route](), List[Request]())
    )({
      case ((routes, badRequests), request) =>
        LowestRegretRecreate.tryCreateRoute(problem)(request) match {
          case None => (routes, request::badRequests)
          case Some(route) => (route::routes, badRequests)
        }
    })

    (routes, badRequests)
  }
}

package sg.beeline.ui

import akka.actor.{ActorRef, Actor}
import sg.beeline._

abstract class Region {
  def contains(latLng : (Double, Double)) : Boolean
}

case class CircularRegion(val lngLat : (Double, Double), val radiusInMetres : Double) extends Region {
  val xy = Util.toSVY(lngLat)

  println(toString)
  println(xy)

  def contains(xy2 : (Double, Double)) = {
    // val xy2 = Util.toSVY(latLng)

    val (x1,y1) = xy
    val (x2,y2) = xy2

    (x2 - x1)*(x2-x1) + (y2-y1)*(y2-y1) <= radiusInMetres * radiusInMetres
  }

  override def toString = s"${lngLat._2},${lngLat._1} <--> ${radiusInMetres}m"
}

abstract class RoutingControl
abstract class RoutingNotification

case class StartRouting(times : List[Double], regions : Seq[Region])
case class StopRouting() extends RoutingControl
case class CurrentSolution() extends RoutingControl
case class Polyline(indices : List[Int]) extends RoutingControl

case class RoutingStopped() extends RoutingNotification
case class RoutingStarted() extends RoutingNotification

class RouteActor extends Actor {
  var lastResults : Traversable[Route] = List()
  val busStops = Import.getBusStops

  val beelineProblem = {
    val suggestions = sg.beeline.Import.getRequests
      .map(x => new Suggestion(x.start, x.end, 8 * 3600 * 1000)) // Group them all into the same time slot
      .filter(x => x.time >= 8 * 3600 * 1000 && x.time <= 9 * 3600 * 1000)

    new BasicRoutingProblem(busStops, suggestions)
  }
  val beelineRecreate = {
    new BeelineRecreate(beelineProblem, beelineProblem.requests)
  }

  def receive = {
    case StartRouting(times, regions) =>
      val suggestions = sg.beeline.Import.getRequests
        .filter(x => times.contains(x.time) && regions.exists(_.contains(x.end)))
        .map(x => new Suggestion(x.start, x.end, 8 * 3600 * 1000)) // Group them all into the same time slot
      val problem = new BasicRoutingProblem(busStops, suggestions)

      val algorithm = new BasicRoutingAlgorithm(problem)

      context.become(algorithm.solve(context, (routes) => this.lastResults = routes), discardOld = false)

      sender ! RoutingStarted

    case StopRouting =>
      sender ! RoutingStopped

    case CurrentSolution =>
      sender ! lastResults

    case SuggestRequest(sLat, sLng, eLat, eLng, time) =>
      sender ! beelineRecreate.findRelated(
        new Request(
          beelineProblem,
          Util.toSVY((sLng, sLat)),
          Util.toSVY((eLng, eLat)),
          time
        )
      ).toList
  }
}

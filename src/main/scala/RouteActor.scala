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

case class StartRouting(time : Double, regions : Seq[Region])
case class StopRouting() extends RoutingControl
case class CurrentSolution() extends RoutingControl
case class Polyline(indices : List[Int]) extends RoutingControl

case class RoutingStopped() extends RoutingNotification
case class RoutingStarted() extends RoutingNotification

class RouteActor extends Actor {
  var lastResults : Traversable[Route] = List()
  val busStops = Import.getBusStops

  def receive = {
    case StartRouting(time, regions) =>
      val suggestions = sg.beeline.Import.getRequests.filter(x => x.time == time && regions.exists(_.contains(x.end)))
      val problem = new BasicRoutingProblem(busStops, suggestions)

      val algorithm = new BasicRoutingAlgorithm(problem)

      context.become(algorithm.solve(context, (routes) => this.lastResults = routes), discardOld = false)

      sender ! RoutingStarted

    case StopRouting =>
      sender ! RoutingStopped

    case CurrentSolution =>
      sender ! lastResults

  }
}

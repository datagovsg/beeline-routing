package sg.beeline
import Util.Point

class Request(val routingProblem : RoutingProblem,
              val start: Point, val end: Point, val time: Double) {

  val startStops = routingProblem.nearBusStops(start).toIndexedSeq
  val endStops = routingProblem.nearBusStops(end).toIndexedSeq

  val startStopsSet = startStops.toSet
  val endStopsSet = endStops.toSet

  override def toString = (Util.toWGS(start), Util.toWGS(end), time).toString
}

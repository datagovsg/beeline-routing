package sg.beeline
import Util.Point

class Request(val routingProblem : RoutingProblem,
              val start: Point, val end: Point, val actualTime: Double,
              val weight : Int = 1) {

  val time = 8*3600*1000
  val startStops = routingProblem.nearBusStopsStart(start).toIndexedSeq
  val endStops = routingProblem.nearBusStopsEnd(end).toIndexedSeq

  val startStopsSet = startStops.toSet
  val endStopsSet = endStops.toSet

  override def toString = (Util.toWGS(start), Util.toWGS(end), time).toString
}

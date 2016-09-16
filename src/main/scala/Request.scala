package sg.beeline
import Util.Point

class Request(val routingProblem : RoutingProblem,
              val start: Point, val end: Point, val time: Double) {

  val startStops = routingProblem.nearBusStops(start).take(1)

  val endStops = routingProblem.nearBusStops(end).take(1)
}

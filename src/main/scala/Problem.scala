package sg.beeline

import Util.Point

trait RoutingProblem {
  def distance(a : BusStop, b : BusStop) : Double

  def nearBusStops(p: Point) : Seq[BusStop]
}

class BusStop(val coordinates: Util.Point, val description: String, roadName: String) {
  // Save the index for caching!
  var index =0

  val xy = Util.toSVY(coordinates)

  override def toString =
    s"BusStop(${xy._1}, ${xy._2}) ${description}"
}

class Suggestion(val start: Util.Point, val end: Util.Point, val time: Int) {
  override def toString =
    s"Suggestion(${start._1}, ${start._2}) to (${end._1}, ${end._2}) @${time}"
}

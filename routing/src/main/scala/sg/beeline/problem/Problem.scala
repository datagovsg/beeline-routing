package sg.beeline.problem

import sg.beeline.util.Util
import sg.beeline.util.Util.Point

trait RoutingProblem {
  def distance(a : BusStop, b : BusStop) : Double
  def nearBusStopsStart(p: Point) : Seq[BusStop]
  def nearBusStopsEnd(p: Point) : Seq[BusStop]

  def initialize: (Traversable[Route], Traversable[Request], Traversable[Request])
}

case class BusStop(
  coordinates: Util.Point,
  heading: Double,
  description: String,
  roadName: String,
  index: Int) {
  // Save the index for caching!
  lazy val xy = Util.toSVY(coordinates)

  override def toString =
    s"BusStop(${coordinates._2},${coordinates._1}) ${description}"
}

case class MrtStation(coordinates: Util.Point, heading: Double, description: String, roadName: String, index: Int) {
  // Save the index for caching!
  val xy = Util.toSVY(coordinates)

  override def toString =
    s"MrtStation(${coordinates._2},${coordinates._1}) ${description}"
}

case class Suggestion(id: Int, start: Util.Point, end: Util.Point, actualTime: Double, weight : Int = 1) {
  val time = actualTime

  override def toString =
    s"Suggestion(${start._1}, ${start._2}) to (${end._1}, ${end._2}) @${time}"
}


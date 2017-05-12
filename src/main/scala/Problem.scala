package sg.beeline

import Util.Point
import akka.actor.ActorRef
import sg.beeline.ui.RoutingControl

trait RoutingProblem {
  def distance(a : BusStop, b : BusStop) : Double
  def nearBusStopsStart(p: Point) : Seq[BusStop]
  def nearBusStopsEnd(p: Point) : Seq[BusStop]

  def initialize: (Traversable[Route], Traversable[Request], Traversable[Request])
}

class BusStop(val coordinates: Util.Point, val heading: Double, val description: String, roadName: String, val index: Int) {
  // Save the index for caching!
  val xy = Util.toSVY(coordinates)

  override def toString =
    s"BusStop(${coordinates._2},${coordinates._1}) ${description}"
}

class MrtStation(val coordinates: Util.Point, val heading: Double, val description: String, roadName: String, val index: Int) {
  // Save the index for caching!
  val xy = Util.toSVY(coordinates)

  override def toString =
    s"MrtStation(${coordinates._2},${coordinates._1}) ${description}"
}

class Suggestion(val start: Util.Point, val end: Util.Point, val actualTime: Double, val weight : Int = 1) {
  val time = actualTime

  override def toString =
    s"Suggestion(${start._1}, ${start._2}) to (${end._1}, ${end._2}) @${time}"
}

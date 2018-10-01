package sg.beeline.problem

import java.sql.Timestamp

import sg.beeline.ruinrecreate.BeelineRecreateSettings
import sg.beeline.util.Util
import sg.beeline.util.Util.Point

trait RoutingProblem {
  def distance(a : BusStop, b : BusStop) : Double
  def initialize: (Traversable[Route], Traversable[Request], Traversable[Request])
  def nearBusStops(point : Point, maxDistance: Double): Seq[BusStop]
  def settings: BeelineRecreateSettings
}

case class BusStop(
  coordinates: Util.Point,
  heading: Double,
  description: String,
  roadName: String,
  index: Int,
  stopCode: Option[String] = None) {
  // Save the index for caching!
  lazy val xy = Util.toSVY(coordinates)

  override def toString =
    s"BusStop(${coordinates._2},${coordinates._1}) ${description}"
}

case class Suggestion(id: Int, start: Util.Point, end: Util.Point, time: Double,
                      weight : Int = 1, createdAt: Long, userId: Option[Int],
                      email: Option[String], daysOfWeek: Int) {
  lazy val startLngLat = Util.toWGS(start)
  lazy val endLngLat = Util.toWGS(end)

  override def toString =
    s"Suggestion(${start._1}, ${start._2}) to (${end._1}, ${end._2}) @${time}"
}


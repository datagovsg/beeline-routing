package sg.beeline.problem

import sg.beeline.io.Import
import sg.beeline.util.Util
import sg.beeline.util.Util.Point

import scala.math.min

trait Request {
  def routingProblem : RoutingProblem
  def start: Point
  def end: Point
  def time: Double
  def actualTime: Double
  def weight : Int

  lazy val startStops = routingProblem.nearBusStopsStart(start).toIndexedSeq
  lazy val endStops = routingProblem.nearBusStopsEnd(end).toIndexedSeq

  lazy val startStopsSet = startStops.toSet
  lazy val endStopsSet = endStops.toSet

  lazy val startWGS = Util.toWGS(start)
  lazy val endWGS = Util.toWGS(end)

  // Everything relating to distance here is in metres, and points here is in (lat, lon)
  lazy val distanceFromNearestMrt : Double = {
    val distances = Import.getMrtStations.map(mrtStation => Util.computeDistance(mrtStation.coordinates, startWGS))
    val minDist = distances.foldLeft(Double.PositiveInfinity)(min(_,_))
    minDist
  }

  def getWeightByDistanceToMrt(maxDistanceFromMrt : Double, minProbabilityAtMrt : Double) : Double = {
    val correspondingWeight = {
      if (distanceFromNearestMrt <= maxDistanceFromMrt)
        weight * minProbabilityAtMrt
      else
        weight
    }
    correspondingWeight
  }

  override def toString: String = (Util.toWGS(start), Util.toWGS(end), time).toString
}

object Request {
  class RequestFromSuggestion(val routingProblem : RoutingProblem,
                              val suggestion: Suggestion,
                              val routeTime: Double) extends Request {
    override val start: (Double, Double) = suggestion.start
    override val end: (Double, Double) = suggestion.end
    override val time: Double = routeTime
    override val actualTime: Double = suggestion.time
    override val weight: Int = suggestion.weight
  }
}

class BasicRequest(val routingProblem: RoutingProblem,
                   val start: (Double, Double),
                   val end: (Double, Double),
                   val time: Double,
                   val weight: Int = 1
                  ) extends Request {
  override val actualTime: Double = time
}
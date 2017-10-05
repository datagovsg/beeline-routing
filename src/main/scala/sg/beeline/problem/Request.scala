package sg.beeline.problem

import sg.beeline.io.Import
import sg.beeline.util.Util
import sg.beeline.util.Util.Point

import scala.math.min

class Request(val routingProblem : RoutingProblem,
              val start: Point, val end: Point, val actualTime: Double,
              val weight : Int = 1) {

  val time = 8*3600*1000
  val startStops = routingProblem.nearBusStopsStart(start).toIndexedSeq
  val endStops = routingProblem.nearBusStopsEnd(end).toIndexedSeq

  val startStopsSet = startStops.toSet
  val endStopsSet = endStops.toSet

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

  override def toString = (Util.toWGS(start), Util.toWGS(end), time).toString
}

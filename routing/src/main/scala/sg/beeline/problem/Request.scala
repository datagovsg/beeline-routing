package sg.beeline.problem

import sg.beeline.io.DataSource
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

  def dataSource: DataSource

  lazy val startStops = routingProblem.nearBusStopsStart(start).toIndexedSeq
  lazy val endStops = routingProblem.nearBusStopsEnd(end).toIndexedSeq

  lazy val startStopsSet = startStops.toSet
  lazy val endStopsSet = endStops.toSet

  lazy val startWGS = Util.toWGS(start)
  lazy val endWGS = Util.toWGS(end)

  override def toString: String = (Util.toWGS(start), Util.toWGS(end), time).toString
}

object Request {
  class RequestFromSuggestion(val routingProblem : RoutingProblem,
                              val suggestion: Suggestion,
                              val routeTime: Double,
                              val dataSource: DataSource) extends Request {
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
                   val weight: Int = 1,
                   val dataSource: DataSource
                  ) extends Request {
  override val actualTime: Double = time
}
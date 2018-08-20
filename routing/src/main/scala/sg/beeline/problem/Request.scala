package sg.beeline.problem

import sg.beeline.io.DataSource
import sg.beeline.problem.Request.RequestOverrideTime
import sg.beeline.util.Util
import sg.beeline.util.Util.Point

import scala.math.min

trait Request {
  def routingProblem : RoutingProblem
  def start: Point
  def end: Point
  def time: Double
  def weight : Int

  def dataSource: DataSource

  lazy val startStops = routingProblem.nearBusStopsStart(start).toIndexedSeq
  lazy val endStops = routingProblem.nearBusStopsEnd(end).toIndexedSeq

  lazy val startStopsSet = startStops.toSet
  lazy val endStopsSet = endStops.toSet

  lazy val startWGS = Util.toWGS(start)
  lazy val endWGS = Util.toWGS(end)

  override def toString: String = (Util.toWGS(start), Util.toWGS(end), time).toString

  def withTime(time: Double) = new RequestOverrideTime(this, time)
}

object Request {
  class RequestFromSuggestion(
                               val suggestion: Suggestion,
                               val routingProblem : RoutingProblem,
                                val dataSource: DataSource) extends Request {
    override val start: (Double, Double) = suggestion.start
    override val end: (Double, Double) = suggestion.end
    override val time: Double = suggestion.time
    override val weight: Int = suggestion.weight

    override def hashCode: Int =
      List(suggestion, routingProblem, dataSource)
          .foldLeft(1) {
            case (hashCode, o) =>
              hashCode * 41 + o.hashCode
          }

    override def equals(obj: scala.Any): Boolean =
      obj match {
        case rfs: RequestFromSuggestion =>
          rfs.suggestion == suggestion &&
          rfs.dataSource == dataSource &&
          rfs.routingProblem == routingProblem
        case _ =>
          false
      }
  }

  class RequestOverrideTime(var r: Request,
                            val time: Double) extends Request {
    override val start: (Double, Double) = r.start
    override val end: (Double, Double) = r.end
    override val weight: Int = r.weight
    override val routingProblem: RoutingProblem = r.routingProblem
    override val dataSource: DataSource = r.dataSource
  }
}

class BasicRequest(val routingProblem: RoutingProblem,
                   val start: (Double, Double),
                   val end: (Double, Double),
                   val time: Double,
                   val weight: Int = 1,
                   val dataSource: DataSource,
                   val id: Int
                  ) extends Request {
}
package sg.beeline.problem

import sg.beeline.io.DataSource
import sg.beeline.problem.Request.RequestOverrideTime
import sg.beeline.util.{Projections, Point}

trait Request {
  def routingProblem : RoutingProblem
  def start: Point
  def end: Point
  def time: Double
  def weight : Int
  def id: Int

  def dataSource: DataSource

  private def busStopsWithinRadiusOrNearest(point: Point, distance: Double): IndexedSeq[BusStop] = {
    val stopsWithinRadius = routingProblem.nearBusStops(point, distance)

    if (stopsWithinRadius.nonEmpty) stopsWithinRadius.toArray[BusStop]
    else Array(routingProblem.nearestBusStop(point))
  }

  lazy val startStops = busStopsWithinRadiusOrNearest(start, routingProblem.settings.startWalkingDistance)
  lazy val endStops = busStopsWithinRadiusOrNearest(end, routingProblem.settings.endWalkingDistance)

  lazy val startStopsSet = startStops.toSet
  lazy val endStopsSet = endStops.toSet

  lazy val startWGS = Projections.toWGS(start)
  lazy val endWGS = Projections.toWGS(end)

  override def toString: String = (Projections.toWGS(start), Projections.toWGS(end), time).toString

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
    override val id: Int = suggestion.id

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

  class RequestOverrideTime(val r: Request,
                            val time: Double) extends Request {
    override val start: (Double, Double) = r.start
    override val end: (Double, Double) = r.end
    override val weight: Int = r.weight
    override val routingProblem: RoutingProblem = r.routingProblem
    override val dataSource: DataSource = r.dataSource
    override val id: Int = r.id
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
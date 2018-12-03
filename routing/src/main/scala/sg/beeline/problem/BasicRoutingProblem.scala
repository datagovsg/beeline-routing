package sg.beeline.problem

import com.thesamet.spatial.KDTreeMap
import sg.beeline.io.DataSource
import sg.beeline.ruinrecreate.{BeelineRecreateSettings, DirectFerryRecreate}
import sg.beeline.util.{Point, KDTreeQuery}

class BasicRoutingProblem(val suggestions: Seq[Suggestion],
                          val dataSource: DataSource,
                          override val settings: BeelineRecreateSettings)
  extends RoutingProblem {

  type BusStopsTree = KDTreeMap[(Double, Double), BusStop]

  val busStopsTree : BusStopsTree = KDTreeMap.fromSeq(
    dataSource.busStops map {x => x.xy -> x}
  )

  val requests : Seq[Request] = suggestions.map(sugg =>
    new Request.RequestFromSuggestion(sugg, this, dataSource))
    .filter(_.startStops.nonEmpty)
    .filter(_.endStops.nonEmpty)

  // The current set of routes for the current iteration
  def distance(a : BusStop, b: BusStop) : Double = dataSource.distanceFunction(a, b)

  override def nearBusStops(point : Point, maxDistance: Double) =
    KDTreeQuery.queryBall(busStopsTree, point, maxDistance)
    .sortBy(_._1)
    .map(_._2)

  override def nearestBusStop(point : Point) =
    busStopsTree.findNearest(point, 1)
      .head._2

  // Start with a solution where everyone is ferried directly from the nearest
  def initialize = {
    val (routes, badRequests) = DirectFerryRecreate.recreate(this, List(), requests)

    (routes, requests, badRequests)
  }

  def solution = Array[String]()
}

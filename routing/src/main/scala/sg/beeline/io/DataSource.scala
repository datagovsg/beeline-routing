package sg.beeline.io

import sg.beeline.problem.BusStop

case class BusStopSchema(Latitude: Double, Longitude: Double, Heading: Option[Double], Description: String, RoadName: String)

/**
 * Override `busStops`, `distanceFunction` and `mrtStations`.
 *
 * `distanceFunction` and `busStops` are placed together because
 * they may be tightly coupled (e.g. the same `distanceFunction`
 * may not be able to handle bus stops from a different
 * data source.
 */
trait DataSource {
  def busStops: Seq[BusStop]
  def distanceFunction(a: BusStop, b: BusStop): Double

  lazy val busStopsByIndex = Map(
    busStops.map(b => (b.index, b)) : _*
  )
}

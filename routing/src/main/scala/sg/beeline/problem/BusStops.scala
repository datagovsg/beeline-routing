package sg.beeline.problem

case class BusStops(busStops: Seq[BusStop],
                    distanceFunction: Function2[BusStop, BusStop, Double]) {
  lazy val busStopsByIndex = Map(
    busStops.map(b => (b.index, b)) : _*
  )
}

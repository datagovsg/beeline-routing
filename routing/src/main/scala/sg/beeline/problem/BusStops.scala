package sg.beeline.problem

case class BusStops(busStops: Seq[BusStop],
                    distanceFunction: Function2[Int, Int, Double])

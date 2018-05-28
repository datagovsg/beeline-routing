import sg.beeline._
import sg.beeline.io.Import
import sg.beeline.problem.BusStop
import sg.beeline.util.Geo

object ODC {
  val busStops = Import.getBusStops.view
  val ods = busStops.flatMap(o => busStops.map(d => (o,d)))
//  ods.length
  // Distance cache
  val distanceMatrix = Import.distanceMatrix
  //
  def travelTime(stops : BusStop*) = {
    val timeMs = stops.sliding(2).map({
      case Seq(s1, s2) =>
        distanceMatrix(s1.index)(s2.index) +
          (if (s1 != s2) 0.0 else 0.0)
    }).sum
    timeMs / 60000
  }

  // Find compatible detours...
//  val compatible = ods.flatMap({
//    case (o, d) =>
//      busStops.find(s =>
//        travelTime(o, s, d) - travelTime(o,d) < -1
//      ) match {
//        case Some(stop) => Some((o, stop, d))
//        case None => None
//      }
//  })
  lazy val odArray = {
    val arr = Array.ofDim[Int](ods.size, 2)

    ods.zipWithIndex.foldLeft(null)({
      case (acc, ((o,d), i)) => {
        arr(i)(0) = o.index
        arr(i)(1) = d.index
        null
      }
    })

    arr
  }

//  compatible.map({
//    case (o,s,d) =>
//      List(o,s,d).map(s => (s.index, s.heading))
////      List(
////        Geo.travelPath(o.coordinates, o.heading, d.coordinates, d.heading),
////        Geo.travelPath(o.coordinates, o.heading, s.coordinates, s.heading),
////        Geo.travelPath(s.coordinates, s.heading, d.coordinates, d.heading)
////      ).map("[" + _.map({case (b,a) => s"[${a}, ${b}]"}).mkString(",") + "]")
////        .mkString("\n")
//  }).size
  val z = busStops.find(x => x.index == 0).orNull
  val a = busStops.find(x => x.index == 53).orNull
  val b = busStops.find(x => x.index == 138).orNull

  Geo.penalizedTravelTime(
    z.coordinates, z.heading,
    b.coordinates, b.heading
  ) / 60000
  Geo.penalizedTravelTime(
    z.coordinates, z.heading,
    a.coordinates, a.heading
  ) / 60000
  Geo.penalizedTravelTime(
    a.coordinates, a.heading,
    b.coordinates, b.heading
  ) / 60000
  Geo.travelPathStr(z.coordinates, z.heading, b.coordinates, b.heading)
  Geo.travelPathStr(z.coordinates, z.heading, a.coordinates, a.heading)

  Geo.travelPathStr(a.coordinates, a.heading, b.coordinates, b.heading)
  (a.heading, b.heading, z.heading)

  //
//  ods.map({
//    case (o,d) =>
//      busStops.count(s =>
//        travelTime(o,s,d) - travelTime(o,d) < -1
//      )
//  }).foldLeft(
//    (0, 0)
//  ) ((acc, current) => {
//    val (runningSum, count) = acc
//    println(s"Avg so far: ${runningSum.toDouble / count}")
//
//    (runningSum + current, count + 1)
//  })
//d d
//
//  1+4
////  compatible.sum.toDouble / compatible.size
//
//  1+5
}
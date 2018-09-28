package sg.beeline

import java.io.{FileOutputStream, ObjectOutputStream}
import java.util.zip.GZIPOutputStream

import com.graphhopper.GraphHopper
import com.graphhopper.util.CmdArgs
import sg.beeline.io.BuiltIn
import sg.beeline.problem.BusStop
import sg.beeline.util.Geo

object DistanceMatrixCachingApp extends App {
  if (args(0) == "cache") {
    cache()
  } else if (args(0) == "initialize-geo") {
    Geo.initialize()
  }

  def cache() {
    val busStops = BuiltIn.busStops

    val distanceMatrix = {
      val m = Array.ofDim[Double](busStops.size, busStops.size)
      val busStopsArr = busStops.toArray

      val indices = for (i <- busStopsArr.indices;
                          j <- busStopsArr.indices) yield (i,j);
      Geo.initialize();
      indices.par.foreach({ case (i,j) =>
        if (j == 0)
          println(s"Bus stops for ... ${i}")

        m(i)(j) = Geo.penalizedTravelTime(
          busStopsArr(i).coordinates,
          busStopsArr(i).heading,
          busStopsArr(j).coordinates,
          busStopsArr(j).heading
        )

        if (m(i)(j) == Double.PositiveInfinity) {
          println(("+INF", busStopsArr(i).coordinates, busStopsArr(j).coordinates))
        }
      })
      m
    }

    val oos = new java.io.ObjectOutputStream(
      new java.util.zip.GZIPOutputStream(
        new java.io.FileOutputStream("./distances_cache.dat.gz")))

    oos.writeObject(distanceMatrix)
    oos.close()
  }
}

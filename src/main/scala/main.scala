package sg.beeline

import java.io.{FileOutputStream, ObjectOutputStream}
import java.util.zip.GZIPOutputStream

import scala.collection.SeqView
import scala.util.Try

object Hi {
  def main(args: Array[String]) {
    if (args.length == 0 || args(0) == "web") {
      web()
    }
    else if (args(0) == "cache") {
      cache()
    }
    else if (args(0) == "estimate") {
      estimate()
    }
    else if (args(0) == "zmq") {
      ZmqRoutingService.begin()
    }
    else require(false)
  }

  def cache() {
    val busStops = Import.getBusStops

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

  def web() {
    import akka.io.IO
    import akka.actor._
    import spray.can.Http

    implicit val system = ActorSystem()

    val service = system.actorOf(Props[IntelligentRoutingService], "intelligent-routing")

    Geo.initialize()

    IO(Http) ! Http.Bind(service,
      interface = "0.0.0.0",
      port = scala.util.Properties.envOrElse("PORT", "8080").toInt)
  }

  def estimate(): Unit = {
    val busStops = Import.getBusStops.toArray
    // Distance cache
    val distanceMatrix = Import.distanceMatrix
    //
    def travelTime(stops : BusStop*) = {
      val timeMs = stops.sliding(2).map({
        case Seq(s1, s2) =>
          distanceMatrix(s1.index)(s2.index) +
            (if (s1 != s2) 60000.0 else 0.0)
      }).sum
      timeMs / 60000
    }

    def shortDetours(od : (BusStop, BusStop)) = od match {
      case (o,d) =>
        busStops.filter(s => {
          val detour = travelTime(o, s, d) - travelTime(o,d)
          !detour.isInfinity && detour < 1.6
        })
    }
    def shortDetoursIndices(od : (Int, Int)) = od match {
      case (o,d) =>
        val oBs = busStops(o)
        val dBs = busStops(d)

        busStops.indices.filter(s => {
          val detour = travelTime(oBs, busStops(s), dBs) - travelTime(oBs, dBs)
          !detour.isInfinity && detour < 1.6
        })
    }

    println("Done loading bus stops")

    // Find compatible detours...
    lazy val (odArray, compatibilityArray) = {
      val numOds = busStops.length * busStops.length
      val ods = 0 until numOds
      val odArr = Array.ofDim[Int](numOds, 2)
      val compatArr = Array.ofDim[Array[Int]](numOds)

      ods.foreach(i => {
        odArr(i)(0) = i / busStops.length
        odArr(i)(1) = i % busStops.length
      })
      println("Done assigning indices")

      odArr.indices.par.foreach(i => {
        if (i % 1000 == 0)
            println (i)

        val o = odArr(i)(0)
        val d = odArr(i)(1)

        compatArr(i) = shortDetoursIndices((o, d)).toArray
      })

      (odArr, compatArr)
    }

    println(odArray(100))

    {
      val oos = new ObjectOutputStream(
        new GZIPOutputStream(
          new FileOutputStream("./compatibility-cache.dat.gz")
        )
      )

      oos.writeObject(odArray)
      oos.writeObject(compatibilityArray)
      oos.close()
    }
  }

}

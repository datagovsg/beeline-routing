package sg.beeline

object Hi {
  def main(args: Array[String]) {
    if (args.length == 0 || args(0) == "web") {
      web()
    }
    else if (args(0) == "cache") {
      cache()
    }
    else require(false)
  }

  def cache() {
    val busStops = Import.getBusStops

    val distanceMatrix = {
      val m = Array.ofDim[Double](busStops.size, busStops.size)
      val busStopsArr = busStops.toArray

      val indices = for (i <- 0 until busStopsArr.size;
                         j <- 0 until busStopsArr.size) yield (i,j);
      Geo.initialize();
      indices.par.foreach({ case (i,j) =>
        if (j == 0)
          println(s"Bus stops for ... ${i}")

        m(i)(j) = Geo.travelTime(
          busStopsArr(i).coordinates,
          busStopsArr(j).coordinates
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

    IO(Http) ! Http.Bind(service, interface = "localhost", port = 8080)
  }

}

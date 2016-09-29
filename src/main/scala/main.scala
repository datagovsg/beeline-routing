package sg.beeline

object Hi {
  def main(args: Array[String]) {
    println(args.length)
    for (arg <- args) {
      println(arg)
    }
    if (args.length == 0 || args(0) == "route") {
      web()
      route()
    }
    else if (args(0) == "cache") {
      cache()
    }
    else if (args(0) == "web") {
      web()
    }
    else require(false)
  }

  def cache() {
    val busStops = Import.getBusStops

    val distanceMatrix = {
      val m = Array.ofDim[Double](busStops.size, busStops.size)
      val busStopsArr = busStops.toArray

      for (i <- 0 until busStopsArr.size;
           j <- 0 until busStopsArr.size) {

        if (j == 0)
          println(s"Bus stops for ... ${i}")

        m(i)(j) = Geo.travelTime(
          busStopsArr(i).coordinates,
          busStopsArr(j).coordinates
        )
      }
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

  def route() {
    val problem = new BasicRoutingProblem(Import.getBusStops, Import.getRequests)

    def iterSolution(routes : List[Route], badRequests : List[Request])
      : (List[Route], Seq[Request]) = {
        // Ruin
        val (preservedRoutes, unservedRequests) = Ruin.ruin(problem, routes, badRequests)

        // Recreate
        val (newRoutes, newBadRequests) = Recreate.recreate(problem, preservedRoutes, unservedRequests)

        (newRoutes, newBadRequests)
      }

    val (routes, requests, badRequests) = problem.initialize
    val (newRoutes, newBadRequests, score) = (0 until 10)
      .foldLeft(
          (routes.toList, badRequests, Double.NegativeInfinity)
        )((acc, iterCount) => {
        val (routeList, requestList, previousScore) = acc
        val (nextRouteList, nextRequests) = iterSolution(routeList, badRequests)
        val nextScore = Score.score(nextRouteList)

        // FIXME: Use Threshold Annealing
        if (nextScore > previousScore) {
          println(s"Score decreased to ${nextScore}")

          // Make the routes available for web access
          CurrentSolution.updateRoutes(nextRouteList)

          (nextRouteList, requestList, nextScore)
        }
        else {
          println(s"Score maintained at ${previousScore}")
          (routeList, requestList, previousScore)
        }
      })

    SolutionPrinter.writeProblem(requests)
    SolutionPrinter.writeLocations(problem.busStops)
    SolutionPrinter.writeSolution(newRoutes)

    println(newBadRequests)
  }
}

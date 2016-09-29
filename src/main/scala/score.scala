package sg.beeline
object Score {

  // FIXME: A better scoring function!
  def score(routes: Seq[Route]) : Double = {

    return -meanTravelTime(routes) + numPassengers(topNRoutes(30)(routes))
  }

  private def meanTravelTime(routes: Seq[Route]) = {
    val travelTimes = routes.flatMap(r =>
      r.activitiesWithTimes.toStream.flatMap(x => x._1 match {
        case Pickup(req, loc) => Some((req, x._3))
        case Dropoff(req, loc) => Some((req, x._3))
        case _ => None
      })
        .groupBy(_._1) // Group by similar requests
        .map(pair => {
          val times = pair._2.map(_._2) // Extract the max times
          require(times.size == 2)
          times.max - times.min // Find the time difference
        })
      )

    travelTimes.sum / travelTimes.size / 60000.0
  }

  private def numVehicles(routes: Seq[Route]) = routes.size

  private def numPassengers(routes: Seq[Route]) =
    routes.map(_.activities.size / 2 - 1).sum

  private def topNRoutes(N: Int)(routes : Seq[Route]) = {
    routes.sortBy(_.activities.size).takeRight(N)
  }
}

// ||-ize recreate step
// restrict by region
// compute the actual routing time (and cache the results)
// add more constraints -- to ensure that routes are feasible, not too much detour

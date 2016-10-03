package sg.beeline

trait Score {
  def score(routes: Traversable[Route]) : Double
}

object Score extends Score {
  // FIXME: A better scoring function!
  // The higher the better
  def score(routes: Traversable[Route]) : Double = {
    return +numPassengers(topNRoutes(30)(routes))
  }

  private def meanTravelTime(routes: Traversable[Route]) = {
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

  private def numVehicles(routes: Traversable[Route]) = routes.size

  private def numPassengers(routes: Traversable[Route]) =
    routes.map(_.activities.size / 2 - 1).sum

  private def topNRoutes(N: Int)(routes : Traversable[Route]) = {
    routes.toSeq.sortBy(_.activities.size).takeRight(N)
  }
}

// restrict by region
// compute the actual routing time (and cache the results)
// Add more constraints -- limit the maximum detour of the routes

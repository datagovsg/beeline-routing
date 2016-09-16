package sg.beeline
object Score {

  // FIXME: A better scoring function!
  def score(routes: Seq[Route]) : Double = {
    // val sumOfTravelTime = routes.map(r => - r.maxPossibleTimes.head + r.maxPossibleTimes(r.maxPossibleTimes.size - 2)).sum
    //
    // -sumOfTravelTime / 60000
    // -routes.size

    // Another way to score is to...
    // Take the top 20 routes and count the number of passengers
    routes.sortBy(r => - r.activities.size).take(20)
      .map(r => r.activities.size).sum
  }
}

// ||-ize recreate step
// restrict by region
// compute the actual routing time (and cache the results)
// add more constraints -- to ensure that routes are feasible, not too much detour

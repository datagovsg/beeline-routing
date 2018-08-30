package sg.beeline.problem

object RouteSimilarity {
  def isSimilar(route1 : Route, route2 : Route, similarityThreshold : Double = 0.9): Boolean = {
    // We return true if the requests being served are similar enough.
    // Note: Similar could mean that route1 is almost a subset of, a superset of, or the same as route2
    val servedRequestsSet1 : Set[Int] = route1.requestsInfo.keys.map(_.id).toSet
    val servedRequestsSet2 : Set[Int] = route2.requestsInfo.keys.map(_.id).toSet
    val similarServedRequest : Set[Int] = servedRequestsSet1 intersect servedRequestsSet2
    (similarServedRequest.size.toDouble / math.min(servedRequestsSet1.size, servedRequestsSet2.size)) > similarityThreshold
  }
}

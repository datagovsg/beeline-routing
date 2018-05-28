package sg.beeline.problem

object RouteSimilarity {
  def isSimilar(route1 : Route, route2 : Route, similarityThreshold : Double = 0.9): Boolean = {
    // We return true if the requests being served are similar enough.
    // Note: Similar could mean that route1 is almost a subset of, a superset of, or the same as route2
    val servedRequestsSet1 : Set[Request] = route1.requestsInfo.map(_._1).toSet
    val servedRequestsSet2 : Set[Request] = route2.requestsInfo.map(_._1).toSet
    val similarServedRequest : Set[Request] = servedRequestsSet1 intersect servedRequestsSet2
    (similarServedRequest.size.toDouble / math.min(servedRequestsSet1.size, servedRequestsSet2.size)) > similarityThreshold
  }
}

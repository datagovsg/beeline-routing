package sg.beeline
import com.thesamet.spatial.{KDTreeMap, RegionBuilder}
import Util.Point

class BasicRoutingProblem(val busStops: Seq[BusStop], val suggestions: Seq[Suggestion]) extends RoutingProblem {
  println(s"Problem with ${suggestions.size} suggestions")

  type BusStopsTree = KDTreeMap[(Double, Double), BusStop]

  val busStopsTree : BusStopsTree = KDTreeMap.fromSeq(
    busStops map {x => x.xy -> x}
  )
  val maxDetourRatio = 1.5
  val distance = 400.0

  val requests = suggestions.map(sugg =>
    new Request(this, sugg.start, sugg.end, sugg.time))
    .filter(_.startStops.nonEmpty)
    .filter(_.endStops.nonEmpty)

  println(s"Only ${requests.size} suggestions used")

  val distanceMatrix = {
    val ois = new java.io.ObjectInputStream(
                new java.util.zip.GZIPInputStream(
                  new java.io.FileInputStream("./distances_cache.dat.gz")))

    ois.readObject().asInstanceOf[Array[Array[Double]]]
  }

  // The current set of routes for the current iteration
  def distance(a : BusStop, b: BusStop) : Double = {
    distanceMatrix(a.index)(b.index)
  }

  //
  def nearBusStops(origin : Point) =
    kdtreeQuery.queryBall(busStopsTree, origin, this.distance)
    .sortBy(_._1)
    .map(_._2)

  // Start with a solution where everyone is ferried directly from the nearest
  def initialize = {
    val (routes, badRequests) = requests.foldLeft(
      (List[Route](), List[Request]())
    )({
      case ((routes, badRequests), request) =>
        LowestRegretRecreate.tryCreateRoute(this)(request) match {
          case None => (routes, request::badRequests)
          case Some(route) => (route::routes, badRequests)
        }
    })

    (routes, requests, badRequests)
  }

  def solution = Array[String]()
}

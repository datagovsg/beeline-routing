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
    .filter(!_.startStops.isEmpty)
    .filter(!_.endStops.isEmpty)

  println(s"Only ${requests.size} suggestions used")

  val distanceMatrix = {
    val m = Array.ofDim[Double](busStops.size, busStops.size)
    val busStopsArr = busStops.toArray
    
    for (i <- 0 until busStopsArr.size) {
      busStopsArr(i).index = i
    }
    //
    // for (i <- 0 until busStopsArr.size; j <- 0 until busStopsArr.size) {
    //   if (j == 0)
    //     println(s"Bus stops for ... ${i}")
    //
    //   m(i)(j) = Geo.travelTime(
    //     busStopsArr(i).coordinates,
    //     busStopsArr(j).coordinates
    //   )
    for (i <- 0 until busStopsArr.size; j <- 0 until busStopsArr.size) {
      val dist = Math.sqrt(kdtreeQuery.squaredDistance(
        busStopsArr(i).xy, busStopsArr(j).xy))
      m(i)(j) = dist
      m(j)(i) = dist
    }
    m
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
  // point
  def initialize = {
    // val groupedRoutes = requests.groupBy(req => {
    //   val pStop = req.startStops.head
    //   val dStop = req.endStops.head
    //   (pStop, dStop, req.time)
    // })
    //
    // val routes = groupedRoutes.toSeq.map(p => p match {
    //   case ((pStop, dStop, time), requests) => {
    //     val initialInsertionPoint : (Activity, Activity) = (new StartActivity, new EndActivity)
    //     val initialRoute = new Route(this, List(initialInsertionPoint._1, initialInsertionPoint._2), time)
    //
    //     requests.foldLeft(
    //       (initialRoute, initialInsertionPoint)
    //     )(
    //       (acc, request) => {
    //         val pickup = new Pickup(request, pStop)
    //         val dropoff = new Dropoff(request, dStop)
    //         val newRoute = acc match {
    //           case (route, ip) => route.insert(pickup, dropoff, ip, ip)
    //         }
    //         val newIp = (pickup, dropoff)
    //         (newRoute, newIp)
    //       }
    //     )
    //   }
    // }).map(_._1)

    // Run the recreate algorithm

    val routes = Recreate.recreate(this, List(), requests)

    (routes, requests)
  }

  def solution = Array[String]()
}

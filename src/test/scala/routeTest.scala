package sg.beeline

import org.scalatest._

class RouteSpec extends FlatSpec with Matchers {

  object ZeroDistance {
    val busStops = Array(
      new BusStop((103.8, 1.38), "BS1", "BS Road", 0),
      new BusStop((103.8, 1.38), "BS1", "BS Road", 1)
    )
  }

  class ZeroDistance extends RoutingProblem {
    def distance(a : BusStop, b: BusStop) = 0

    def nearBusStops(p : (Double, Double)) = ZeroDistance.busStops
  }

  class TestActivity(val routingProblem: RoutingProblem, val busStop: BusStop,
      val st : Double, val et : Double, val dt: Double, val svct: Double) extends Pickup(new Request(routingProblem, (0,0), (0,0), 0), busStop) {

    override def minTime = st
    override def maxTime = et
    override def dwellTime = dt
    override def serviceTime = svct
  }

  "Route" should "reflect min/max times correctly (1)" in {
    val problem = new ZeroDistance

    val activities = List(
      new StartActivity,
      new TestActivity(problem, ZeroDistance.busStops(0), 1000, 10000, 0, 0),
      new TestActivity(problem, ZeroDistance.busStops(0), 2000, 9000, 0, 0),
      new TestActivity(problem, ZeroDistance.busStops(0), 3000, 8000, 0, 0),
      new TestActivity(problem, ZeroDistance.busStops(0), 4000, 7000, 0, 0),
      new EndActivity
    )

    val route = new Route(problem, activities, 0)

    route.minPossibleTimes(1) should be (1000)
    route.minPossibleTimes(2) should be (2000)
    route.minPossibleTimes(3) should be (3000)
    route.minPossibleTimes(4) should be (4000)

    route.maxPossibleTimes(1) should be (7000)
    route.maxPossibleTimes(2) should be (7000)
    route.maxPossibleTimes(3) should be (7000)
    route.maxPossibleTimes(4) should be (7000)
  }

  "Route" should "insert activities correctly" in {
    val problem = new ZeroDistance

    val activities = Array(
      new StartActivity,
      new TestActivity(problem, ZeroDistance.busStops(0), 1000, 10000, 0, 0),
      new TestActivity(problem, ZeroDistance.busStops(0), 2000, 9000, 0, 0),
      new TestActivity(problem, ZeroDistance.busStops(0), 3000, 8000, 0, 0),
      new TestActivity(problem, ZeroDistance.busStops(0), 4000, 7000, 0, 0),
      new EndActivity
    )

    val activitiesToInsert = Array(
      new TestActivity(problem, ZeroDistance.busStops(0), 2000, 8000, 0, 0),
      new TestActivity(problem, ZeroDistance.busStops(0), 2000, 7000, 0, 0)
    )

    val route = new Route(problem, activities, 0)

    val newRoute = route.insert(
      activitiesToInsert(0),
      activitiesToInsert(1),
      (activities(1), activities(2)),
      (activities(3), activities(4))
    )

    newRoute.activities(0) should be (activities(0))
    newRoute.activities(1) should be (activities(1))
    newRoute.activities(2) should be (activitiesToInsert(0))
    newRoute.activities(3) should be (activities(2))
    newRoute.activities(4) should be (activities(3))
    newRoute.activities(5) should be (activitiesToInsert(1))
    newRoute.activities(6) should be (activities(4))
    newRoute.activities(7) should be (activities(5))
  }

}

package sg.beeline

import org.scalatest._
import sg.beeline.io.DataSource
import sg.beeline.problem._

class RouteSpec extends FlatSpec with Matchers {

  object counter {
    var i = 0
    def getCounter() = {
      i += 1
      i
    }
  }

  object ZeroDistance extends RoutingProblem {
    val busStops = Array(
      BusStop((103.8, 1.38), 0, "BS1", "BS Road", 0),
      BusStop((103.81, 1.39), 1, "BS2", "BS Road", 1)
    )

    override def distance(a : BusStop, b: BusStop) =
      if (a == b) 0
      else 1500

    override def nearBusStopsStart(p : (Double, Double)) = ZeroDistance.busStops
    override def nearBusStopsEnd(p : (Double, Double)) = ZeroDistance.busStops

    override def initialize = {
      (List(), List(), List())
    }
  }
  val testDataSource = new DataSource {
    override def busStops: Seq[BusStop] = ZeroDistance.busStops

    override def distanceFunction(a: BusStop, b: BusStop): Double = ZeroDistance.distance(a, b)
  }

  class TestActivity(val routingProblem: RoutingProblem, val busStop: BusStop,
      val st : Double, val et : Double, val dt: Double, val svct: Double)
    extends Pickup(new BasicRequest(routingProblem, (0,0), (0,0), 0, 1, testDataSource, id=counter.getCounter()), busStop) {

    override def minTime = st
    override def maxTime = et
    override def dwellTime = dt
    override def serviceTime = svct
  }

  "Route" should "reflect min/max times correctly (1)" in {
    val problem = ZeroDistance

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

    assert {
      route.stops == List(
        ZeroDistance.busStops(0)
      )
    }
  }

  "Route" should "reflect min/max times correctly (2)" in {
    val problem = ZeroDistance

    val activities = List(
      new StartActivity,
      new TestActivity(problem, ZeroDistance.busStops(0), 1000, 15000, 0, 0),
      new TestActivity(problem, ZeroDistance.busStops(1), 2000, 14000, 0, 0),
      new TestActivity(problem, ZeroDistance.busStops(0), 3000, 13000, 0, 0),
      new TestActivity(problem, ZeroDistance.busStops(1), 4000, 12000, 0, 0),
      new EndActivity
    )

    val route = new Route(problem, activities, 0)

    route.minPossibleTimes(1) should be (1000)
    route.minPossibleTimes(2) should be (2500)
    route.minPossibleTimes(3) should be (4000)
    route.minPossibleTimes(4) should be (5500)

    route.maxPossibleTimes(1) should be (7500)
    route.maxPossibleTimes(2) should be (9000)
    route.maxPossibleTimes(3) should be (10500)
    route.maxPossibleTimes(4) should be (12000)

    assert {
      route.stops == List(
        ZeroDistance.busStops(0),
        ZeroDistance.busStops(1),
        ZeroDistance.busStops(0),
        ZeroDistance.busStops(1)
      )
    }
  }

  "Route" should "insert activities correctly" in {
    val problem = ZeroDistance

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

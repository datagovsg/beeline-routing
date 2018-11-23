package sg.beeline

import org.scalatest._
import sg.beeline.io.DataSource
import sg.beeline.problem._
import sg.beeline.ruinrecreate._
import sg.beeline.util.Projections.toSVY
import sg.beeline.util.squaredDistance

import scala.util.Random

class RuinSpec extends FlatSpec with Matchers {
  val TIME = 8 * 3600 * 1000

  // Randomly generate 100 bus stops
  def randomLatLng = {(Math.random() * 0.15 - 0.075 + 103.8, Math.random() * 0.15 - 0.075 + 1.38)}
  val latlngs = for (i <- 0 until 100) yield randomLatLng
  val myBusStops = latlngs.zipWithIndex.map({
    case (ll, i) => BusStop(ll, i, s"BS ${i}", s"R ${i}", i)
    case _ => throw new Error()
  }).toArray

  val perturb : ((Double,Double)) => (Double,Double) =
    {case (x: Double,y :Double) => (x + Math.random() * 0.0001 - 0.00005, y + Math.random() * 0.0001 - 0.00005) }

  val starts = Random.shuffle(latlngs).map(ll => toSVY(perturb(ll)))
  val ends = Random.shuffle(latlngs).map(ll => toSVY(perturb(ll)))

  val requests = (starts zip ends).zipWithIndex.map {case ((s,e), i) =>
    Suggestion(i, s, e, TIME, 1,
      createdAt = 0L, userId = None, email = None, daysOfWeek = 31)
  }

  val testDataSource = new DataSource {
    override def busStops: Seq[BusStop] = myBusStops
    override def distanceFunction(a: BusStop, b: BusStop): Double =
      squaredDistance(a.xy, b.xy) / 11 / 60
  }

  val problem = new BasicRoutingProblem(requests, dataSource = testDataSource, settings = BeelineRecreateSettings.default)

  val (routes, validRequests, badRequests) = problem.initialize

  val (preserved, ruined) = Ruin.ruin(problem, routes.toList, List())

  "BasicRoutingProblem" should "preserve requests on initialize" in {
    assert {badRequests.isEmpty}
    assert {(validRequests intersect badRequests).isEmpty}
    validRequests.toSet should be (problem.requests.toSet)
  }

  "Ruin" should "preserve all requests" in {
    val preservedRequestSet = preserved.flatMap(r => r.activities.flatMap({
      case Pickup(req, loc) => Some(req)
      case Dropoff(req, loc) => Some(req)
      case _ => None
    })).toSet
    val ruinedRequestSet = ruined

    (preservedRequestSet & ruinedRequestSet.toSet).size should be (0)
    (preservedRequestSet ++ ruinedRequestSet) should be (validRequests.toSet)
  }

  "Recreate" should "preserve all requests" in {
    val (recreated, rejected) = Recreate.recreate(problem, preserved, ruined)
    val recreatedRequestSet = recreated.flatMap(r => r.activities.flatMap({
      case Pickup(req, loc) => Some(req)
      case Dropoff(req, loc) => Some(req)
      case _ => None
    })).toSet

    (recreatedRequestSet ++ rejected) should be (validRequests.toSet)
  }

  "LowestRegretRecreate" should "preserve all requests" in {
    val (recreated, rejected) = LowestRegretRecreate.recreate(problem, preserved, ruined)
    val recreatedRequestSet = recreated.flatMap(r => r.activities.flatMap({
      case Pickup(req, loc) => Some(req)
      case Dropoff(req, loc) => Some(req)
      case _ => None
    })).toSet

    (recreatedRequestSet ++ rejected) should be (validRequests.toSet)
  }

  "BeelineRecreate" should "preserve all requests" in {
    val beelineRecreate = new BeelineRecreate(problem, problem.requests)
    val (recreated, rejected) = beelineRecreate.recreate(problem, preserved, ruined)

    val recreatedRequestSet = recreated.flatMap(r => r.activities.flatMap({
      case Pickup(req, loc) => Some(req)
      case Dropoff(req, loc) => Some(req)
      case _ => None
    })).toSet

    (recreatedRequestSet ++ rejected) should be (validRequests.toSet)
  }
}

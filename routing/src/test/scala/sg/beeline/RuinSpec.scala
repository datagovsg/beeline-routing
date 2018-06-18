package sg.beeline

import org.scalatest._
import sg.beeline.io.DataSource
import sg.beeline.problem._
import sg.beeline.ruinrecreate._
import sg.beeline.util.Util.toSVY
import sg.beeline.util.{Util, kdtreeQuery}

import scala.util.Random

class RuinSpec extends FlatSpec with Matchers {
  val TIME = 8 * 3600 * 1000

  // Randomly generate 100 bus stops
  def randomLatLng = {(Math.random() * 0.15 - 0.075 + 103.8, Math.random() * 0.15 - 0.075 + 1.38)}
  val latlngs = for (i <- 0 until 100) yield randomLatLng
  val busStops = latlngs.zipWithIndex.map({
    case (ll, i) => BusStop(ll, i, s"BS ${i}", s"R ${i}", i)
    case _ => throw new Error()
  }).toArray

  val perturb : ((Double,Double)) => (Double,Double) =
    {case (x: Double,y :Double) => (x + Math.random() * 0.0001 - 0.00005, y + Math.random() * 0.0001 - 0.00005) }

  val starts = Random.shuffle(latlngs).map(ll => toSVY(perturb(ll)))
  val ends = Random.shuffle(latlngs).map(ll => toSVY(perturb(ll)))

  val requests = (starts zip ends).zipWithIndex.map({case ((s,e), i) => Suggestion(i, s, e, TIME)})

  val testDataSource = new DataSource {
    override def getMrtStations: Seq[MrtStation] = throw new UnsupportedOperationException
    override def getBusStops: BusStops =
      BusStops(busStops,
        (b1, b2) => kdtreeQuery.squaredDistance(b1.xy, b2.xy) / 11 / 60)
    override def getBusStopsOnly: Seq[BusStop] = busStops
  }

  val problem = new BasicRoutingProblem(requests, dataSource = testDataSource)

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

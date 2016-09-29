package sg.beeline

import org.scalatest._
import scala.util.Random
import Util.toSVY

class RuinSpec extends FlatSpec with Matchers {
  val TIME = 8 * 3600 * 1000

  // Randomly generate 100 bus stops
  def randomLatLng = {(Math.random() * 0.5 - 0.25 + 103.8, Math.random() * 0.5 - 0.25 + 1.38)}
  val latlngs = for (i <- 0 until 100) yield randomLatLng
  val busStops = latlngs.zipWithIndex.map({
    case (ll, i) => new BusStop(ll, s"BS ${i}", s"R ${i}")
    case _ => throw new Error()
  })

  val perturb : ((Double,Double)) => (Double,Double) =
    {case (x: Double,y :Double) => (x + Math.random() * 0.0001 - 0.00005, y + Math.random() * 0.0001 - 0.00005) }

  val starts = Random.shuffle(latlngs).map(ll => toSVY(perturb(ll)))
  val ends = Random.shuffle(latlngs).map(ll => toSVY(perturb(ll)))

  val requests = (starts zip ends).map({case (s,e) => new Suggestion(s,e,TIME)})

  val problem = new BasicRoutingProblem(busStops, requests)

  val (r1, r2, _) = problem.initialize

  val (preserved, ruined) = Ruin.ruin(problem, r1.toList, r2)

  val recreated = Recreate.recreate(problem, preserved, ruined)

  "BasicRoutingProblem" should "preserve requests on initialize" in {
    r2.toSet should be (problem.requests.toSet)
  }

  "Ruin" should "preserve all requests" in {
    val preservedRequestSet = preserved.flatMap(r => r.activities.flatMap({
      case Pickup(req, loc) => Some(req)
      case Dropoff(req, loc) => Some(req)
      case _ => None
    })).toSet
    val ruinedRequestSet = ruined

    println(preservedRequestSet.size)
    println(ruinedRequestSet.size)

    (preservedRequestSet & ruinedRequestSet).size should be (0)
    (preservedRequestSet ++ ruinedRequestSet) should be (r2.toSet)
  }

  "Recreate" should "preserve all requests" in {
    val recreatedRequestSet = recreated._1.map(r => r.activities.map({
      case Pickup(req, loc) => Some(req)
      case Dropoff(req, loc) => Some(req)
      case _ => None
    })).flatten.flatten.toSet

    recreatedRequestSet should be (r2.toSet)
  }

}

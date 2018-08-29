package sg.beeline
import java.util.UUID
import akka.http.scaladsl.model.headers._
import akka.http.scaladsl.model.{HttpMethods, StatusCodes, Uri}
import akka.http.scaladsl.testkit.{RouteTestTimeout, ScalatestRouteTest}
import org.scalatest.FunSuite
import sg.beeline.io.DataSource
import sg.beeline.problem.{BasicRequest, BasicRoutingProblem, BusStop, Suggestion}
import sg.beeline.ruinrecreate.{BeelineRecreateSettings, BeelineSuggestRoute}
import sg.beeline.util.Util
import sg.beeline.web.IntelligentRoutingService
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try
/**
  * Test that we are returning... at least the expected formats?
  */
class BeelineSuggestRouteSpec extends FunSuite with ScalatestRouteTest {
  final private def gridToLngLat(i: Double, j: Double) = {
    val x = 250 * i
    val y = 250 * j
    val originLat = 1.33
    val originLng = 103.78
    val lat = originLat + (y / 6317e3).toDegrees
    val lng = originLng + (x / 6317e3 / math.cos(originLat.toRadians)).toDegrees
    (lng, lat)
  }
  val testDataSource = new DataSource {
    override val busStops: Seq[BusStop] = {
      // Make a rough square grid, 200m apart
      (0 until 50).flatMap(i =>
        (0 until 50).map(j => {
          val (lng, lat) = gridToLngLat(i, j)
          BusStop(
            coordinates = (lng, lat),
            heading = Double.NaN,
            description = s"Bus Stop ($i, $j)",
            roadName = s"East $i West $j",
            index = i * 50 + j
          )
        })
      )
    }
    override def distanceFunction(a: BusStop, b: BusStop) = {
      val (ai, aj) = (a.index / 50, a.index % 50)
      val (bi, bj) = (b.index / 50, b.index % 50)
      // Manhattan distance
      val gridManhattanDistance = math.abs(ai - bi) + math.abs(bj - aj)
      // Assume two minutes between stops
      gridManhattanDistance * 120
    }
  }
  // Make a whole bunch of requests
  val getRequests: Seq[Suggestion] = {
    genericWrapArray(List(8.0, 8.5, 9.0, 9.5)
      .view
      .flatMap({ hour =>
        (0 until 3000).map({ _ =>
          (index: Int) => {
            val randStart = gridToLngLat(
              scala.util.Random.nextDouble() * 20,
              scala.util.Random.nextDouble() * 20)
            val randEnd = gridToLngLat(
              30 + scala.util.Random.nextDouble() * 20,
              30 + scala.util.Random.nextDouble() * 20)
            Suggestion(index, Util.toSVY(randStart), Util.toSVY(randEnd), hour * 3600 * 1000L, 1)
          }
        })
      })
      .zipWithIndex
      .map({ case (f, i) => f(i) })
      .toArray)
  }
  // Some assertions on our assumptions
  require { getRequests.zipWithIndex.forall { case (o, i) => o.id == i} }
  require { testDataSource.busStops.zipWithIndex.forall { case (o, i) => o.index == i} }
  ignore ("BeelineSuggestRoute skips over suggestions without stops") {
    val problem = new BasicRoutingProblem(List(), 500, 500, testDataSource)
    val bsr = new BeelineSuggestRoute(
      problem,
      List(
        new BasicRequest(
          problem,
          Util.toSVY(gridToLngLat(-2, -2)),
          Util.toSVY(gridToLngLat(53, 53)),
          8.5 * 3600e3,
          1,
          testDataSource,
          1
        )
      ),
      new BeelineRecreateSettings(
        startWalkingDistance = 500,
        endWalkingDistance = 500
      )
    )
    val routes = bsr.generatePotentialRoutesFromRequest(
      new BasicRequest(
        problem,
        Util.toSVY(gridToLngLat(1.5, 1.5)),
        Util.toSVY(gridToLngLat(49.5, 49.5)),
        8.5*3600e3,
        1,
        testDataSource,
        2
      )
    )
    assert { routes.nonEmpty && routes.forall { route => route.activities.length == 4 } }
  }
}
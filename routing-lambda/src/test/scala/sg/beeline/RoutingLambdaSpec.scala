package sg.beeline

import java.util.concurrent.ForkJoinPool

import _root_.io.circe.Json
import _root_.io.circe.literal._
import _root_.io.circe.syntax._
import org.scalatest._
import sg.beeline.io.BuiltIn
import sg.beeline.lambda.SuggestRouteHandler
import sg.beeline.problem._
import sg.beeline.ruinrecreate.{BeelineRecreateSettings, BeelineSuggestRouteService}
import sg.beeline.ruinrecreate.BeelineSuggestRouteService.{OD, SuggestRouteInput}
import sg.beeline.util.Util

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

class RoutingLambdaSpec extends FunSuite {

  val problem = new BasicRoutingProblem(
    List(),
    dataSource = BuiltIn,
    settings = BeelineRecreateSettings.default)

  ignore("Route encoder works with empty Activity classes") {
    import sg.beeline.ruinrecreate.BeelineSuggestRouteSerdes._
//    // empty types ie. StartActivity and EndActivity
//    val route = new Route(problem, List(EndActivity()),8.5e7)
//    assert {
//      route.asJson.equals(
//        json"""
//        {
//          "_activities" : [
//            {
//              "type" : "end"
//            }
//          ],
//          "time" : 8.5E7
//        }
//        """)
//    }
  }

  test("Route2 encoder works") {
    import sg.beeline.ruinrecreate.BeelineSuggestRouteSerdes._

    val seedRequest = new BasicRequest(
      problem,
      (21421.649051572367, 32062.31453230393),
      (25959.98999086392, 33974.19460209075),
      8.5 * 3600e3,
      1, BuiltIn, 2
    )

    val route2 = new Route2(problem)(
      IndexedSeq((problem.dataSource.busStops(10), List(seedRequest))),
      IndexedSeq((problem.dataSource.busStops(100), List(seedRequest)))
    )

    val json = route2.asJson

    assert {
      json == json"""
      {
        "pickups": [
          [
            10,
            [
              ${seedRequest.asInstanceOf[Request].asJson}
            ]
          ]
        ],
        "dropoffs": [
          [
            100,
            [
              ${seedRequest.asInstanceOf[Request].asJson}
            ]
          ]
        ]
      }
      """
    }
  }

  test("E2E test") {

    object TestSuggestRouteServiceProxy extends BeelineSuggestRouteService {
      override def requestWithPayload(payload: String)(implicit executionContext: ExecutionContext): Future[Json] = {
        import sg.beeline.ruinrecreate.BeelineSuggestRouteSerdes._
        Future {
          val inputEither = for {
            json <- _root_.io.circe.parser.parse(payload)
            suggestRouteInput <- json.as[SuggestRouteInput]
            suggestRouteOutput <- SuggestRouteHandler.handle(suggestRouteInput, null)
          } yield suggestRouteOutput.asJson

          inputEither.toTry.get
        }
      }
    }
    implicit val executionContext = ExecutionContext.fromExecutor(new ForkJoinPool(50))
    import sg.beeline.ruinrecreate.BeelineSuggestRouteSerdes.route2Encoder
    import _root_.io.circe.syntax._
    val toSVY = (d: Double, e: Double) => Util.toSVY((d, e)).asJson.toString
    val originBusStop = BusStop((21421.649051572367, 32062.31453230393), 100, "Origin", "Hello", 2)
    val destinationBusStop = BusStop((20499.24174394127, 38342.890397198564), 101, "Destination", "Hello Lane", 3)

    val basicRequest1 = new BasicRequest(
      problem, (40778.2186070438, 39589.155309929425), (29899.68739611096, 29292.2330929787), 8.5 * 3600e3, 1, BuiltIn, 3)
    val seedRequest = new BasicRequest(
      problem, (21421.649051572367, 32062.31453230393), (25959.98999086392, 33974.19460209075), 8.5 * 3600e3, 1, BuiltIn, 2
    )
    val requestLambda = Await.result(TestSuggestRouteServiceProxy.executeLambda(
      BeelineRecreateSettings(
        maxDetourMinutes = 2.0
      ),
      seedRequest,
      (originBusStop, destinationBusStop),
      List(
        basicRequest1
      )
    ), Duration.Inf)

    val expected = new Route2(problem)(
      Array((originBusStop, List(seedRequest))),
      Array((destinationBusStop, List(seedRequest))))

    assert { requestLambda.asJson == expected.asJson }
  }
}

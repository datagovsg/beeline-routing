package sg.beeline

import java.util.concurrent.ForkJoinPool

import _root_.io.circe.Json
import _root_.io.circe.literal._
import _root_.io.circe.syntax._
import org.scalatest._
import sg.beeline.io.Import
import sg.beeline.lambda.SuggestRouteHandler
import sg.beeline.problem._
import sg.beeline.ruinrecreate.AWSLambdaSuggestRouteServiceProxy._
import sg.beeline.ruinrecreate.BeelineSuggestRouteService
import sg.beeline.ruinrecreate.BeelineSuggestRouteService.{OD, SuggestRouteInput}
import sg.beeline.util.Util

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

class RoutingLambdaSpec extends FunSuite {

  val problem = new BasicRoutingProblem(List(), 300.0,300.0, dataSource = Import)

  test("Route encoder works with empty Activity classes") {
    // empty types ie. StartActivity and EndActivity
    val route = new Route(problem, List(EndActivity()),8.5e7)
    assert {
      route.asJson.equals(
        json"""
        {
          "_activities" : [
            {
              "type" : "end"
            }
          ],
          "time" : 8.5E7
        }
        """)
    }
  }

  test("E2E test") {

    object TestSuggestRouteServiceProxy extends BeelineSuggestRouteService {
      override def requestWithPayload(payload: String)(implicit executionContext: ExecutionContext): Future[Json] = {
        Future {
          val inputEither = for {
            json <- _root_.io.circe.parser.parse(payload)
            suggestRouteInput <- json.as[SuggestRouteInput]
            suggestRouteOutput <- SuggestRouteHandler.handle(suggestRouteInput, null)

          } yield suggestRouteOutput.asJson

          inputEither match {
            case Right(v) => v
            case Left(exc) => throw exc
          }
        }
      }
    }
    implicit val executionContext = ExecutionContext.fromExecutor(new ForkJoinPool(2))
    import _root_.io.circe.syntax._
    val toSVY = (d: Double, e: Double) => Util.toSVY((d, e)).asJson.toString
    val originBusStop = BusStop((21421.649051572367, 32062.31453230393), 100, "Origin", "Hello", 2)
    val destinationBusStop = BusStop((20499.24174394127, 38342.890397198564), 101, "Destination", "Hello Lane", 3)

    val basicRequest1 = new BasicRequest(
      problem, (40778.2186070438, 39589.155309929425), (29899.68739611096, 29292.2330929787), 8.5 * 3600e3, 1, Import, 2)
    val r = SuggestRouteInput(basicRequest1,
      OD(originBusStop, destinationBusStop),
      List(
        new BasicRequest(
          problem, (21421.649051572367, 32062.31453230393), (25959.98999086392, 33974.19460209075), 8.5 * 3600e3, 1, Import, 2
        )
      ))

    val requestLambda = Await.result(TestSuggestRouteServiceProxy.requestWithPayload(
      r.asJson.toString), Duration.Inf)

    val expected = new Route(
      problem,
      List(
        StartActivity(),
        Pickup(basicRequest1, originBusStop),
        Dropoff(basicRequest1, destinationBusStop),
        EndActivity()),
      8.5 * 3600e3)

    assert (requestLambda.equals(expected.asJson))

  }

}
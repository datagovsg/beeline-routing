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
import sg.beeline.ruinrecreate.BeelineSuggestRouteService.{SuggestRouteInput, SuggestRouteOutput}
import sg.beeline.util.Util

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

class RoutingLambdaSpec extends FunSuite {

  var problem = new BasicRoutingProblem(List(), 300.0,300.0, dataSource = Import)

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
    val r = Await.result(TestSuggestRouteServiceProxy.requestWithPayload(
      s"""
        |{
        |  "seedRequest": {
        |    "id": 2,
        |    "start": [40778.2186070438, 39589.155309929425],
        |    "end": [
        |      29899.68739611096,
        |      29292.2330929787
        |    ],
        |    "time": 3060000,
        |    "weight": 1
        |  },
        |  "od": {
        |    "origin": {
        |      "coordinates": [
        |        21421.649051572367,
        |        32062.31453230393
        |      ],
        |      "heading": 100,
        |      "description": "Origin",
        |      "roadName": "Hello",
        |      "index": 2
        |    },
        |    "destination": {
        |      "coordinates": [
        |        20499.24174394127,
        |        38342.890397198564
        |      ],
        |      "heading": 101,
        |      "description": "Destination",
        |      "roadName": "Hello Lane",
        |      "index": 3
        |    }
        |  },
        |  "requests": [
        |    {
        |      "id": 3,
        |      "start": [
        |         21421.649051572367,
        |         32062.31453230393
        |      ],
        |      "end": [
        |        25959.98999086392,
        |        33974.19460209075
        |      ],
        |      "time": 3060000,
        |      "weight": 1
        |    }
        |  ]
        |}
      """.stripMargin
      ), Duration.Inf)

    assert (r.equals(
      json"""
      {
        "_activities" : [
          {
            "type" : "start"
          },
          {
            "request" : {
              "id" : 2,
              "start" : [
                40778.2186070438,
                39589.155309929425
              ],
              "end" : [
                29899.68739611096,
                29292.2330929787
              ],
              "time" : 3060000.0,
              "weight" : 1
            },
            "_location" : {
              "coordinates" : [
                21421.649051572367,
                32062.31453230393
              ],
              "heading" : 100.0,
              "description" : "Origin",
              "roadName" : "Hello",
              "index" : 2
            },
            "type" : "pickup"
          },
          {
            "request" : {
              "id" : 2,
              "start" : [
                40778.2186070438,
                39589.155309929425
              ],
              "end" : [
                29899.68739611096,
                29292.2330929787
              ],
              "time" : 3060000.0,
              "weight" : 1
            },
            "_location" : {
              "coordinates" : [
                20499.24174394127,
                38342.890397198564
              ],
              "heading" : 101.0,
              "description" : "Destination",
              "roadName" : "Hello Lane",
              "index" : 3
            },
            "type" : "dropoff"
          },
          {
            "type" : "end"
          }
        ],
        "time" : 3060000.0
      }
        """)
    )

  }

}
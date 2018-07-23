package sg.beeline

import java.util.UUID

import org.scalatest.FunSuite
import sg.beeline.io.DataSource
import sg.beeline.problem.{BusStop, Suggestion}
import sg.beeline.web.IntelligentRoutingService
import akka.http.scaladsl.server._
import akka.http.scaladsl.testkit.{RouteTestTimeout, ScalatestRouteTest}
import akka.http.scaladsl.model.headers._
import akka.http.scaladsl.model.{HttpMethods, StatusCodes, Uri}
import sg.beeline.ruinrecreate.BeelineRecreateSettings
import sg.beeline.util.{Util}

import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

/**
 * Test that we are returning... at least the expected formats?
 */
class WebSpec extends FunSuite with ScalatestRouteTest {

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

            Suggestion(index, Util.toSVY(randStart), Util.toSVY(randEnd), hour * 3600 * 1000, 1)
          }
        })
      })
      .zipWithIndex
      .map({ case (f, i) => f(i) })
      .toArray)
  }

  val testService = new IntelligentRoutingService(testDataSource, getRequests).myRoute

  // Some assertions on our assumptions
  require { getRequests.zipWithIndex.forall { case (o, i) => o.id == i} }
  require { testDataSource.busStops.zipWithIndex.forall { case (o, i) => o.index == i} }

  test("/bus_stops fetches all bus stops") {
    Get("/bus_stops") ~> testService ~> check {
      val jarr = _root_.io.circe.parser.parse(responseAs[String]).right.get

      val currentSetOfBusStops = testDataSource.busStops.map(_.description).toSet

      val returnedSetOfBusStops = jarr.asArray.get.map({ jbs =>
        jbs.asObject.get.toMap("description").asString.get
      }).toSet

      assert { currentSetOfBusStops == returnedSetOfBusStops }
    }
  }

  test("/bus_stops/x/y/z fetches bus stops x, y, z") {
    Get("/bus_stops/123/456/789") ~> testService ~> check {
      val jarr = _root_.io.circe.parser.parse(responseAs[String]).right.get
      val idSet = Set(123, 456, 789)
      val returnedListOfBusStops = jarr.asArray.get.map({ jbs =>
        jbs.asObject.get.toMap("index").asNumber.get.toInt.get
      })

      assert { returnedListOfBusStops.size == 3 }
      assert { returnedListOfBusStops.toSet == idSet }
    }
  }

  // TODO: Test for the following
  // - Distance restriction works (all stops are within X m from a request)
  // - Cluster restriction works (all stops are within X m from centre)
  // - Implement time restriction
  test("/routing/begin returns a UUID and polling finally returns a result") {
    import _root_.io.circe.syntax._
    import scala.concurrent.duration._

    implicit val recreateSettingsEncoder = _root_.io.circe.generic
      .semiauto.deriveEncoder[BeelineRecreateSettings]
    implicit val defaultTimeout = RouteTestTimeout(80 seconds)

        // Use a suggestion from the getRequests() --> ensure that at least this one is served
    Get(Uri("/routing/begin").withQuery(Uri.Query(
      "startLat" -> (getRequests(0).startLngLat._2 + 0.0001).toString,
      "startLng" -> getRequests(0).startLngLat._1.toString,
      "endLat" -> getRequests(0).endLngLat._2.toString,
      "endLng" -> getRequests(0).endLngLat._1.toString,
      "time" -> getRequests(0).time.toString,
      "settings" -> _root_.io.circe.Printer.noSpaces.pretty(
        BeelineRecreateSettings(
          maxDetourMinutes = 2.0,
          startClusterRadius = 1500,
          startWalkingDistance = 200,
          endClusterRadius = 1500,
          endWalkingDistance = 200
        ).asJson
      ))
    )) ~> testService ~> check {
      val response = responseAs[String]
      val uuidTry = Try { UUID.fromString(response) }

      assert { uuidTry.isSuccess }

      def tryUntilResult(): Future[String] = {
        Get(Uri("/routing/poll").withQuery(Uri.Query("uuid" -> response))) ~> testService ~>
          check {
            if (status == StatusCodes.Accepted) {
              val delayed = akka.pattern.after(5 second, using = system.scheduler)(Future(()))
              delayed.flatMap(_ => tryUntilResult())
            } else if (status == StatusCodes.OK) {
              val s = responseAs[String]
              Future(s)
            } else {
              Future.failed(new IllegalStateException(s"Unacceptable status code $status"))
            }
          }
      }

      val fut = tryUntilResult().map({ s =>
        // Check schema
        val json = _root_.io.circe.parser.parse(s).right.get
        val jarr = json.asArray.get

        // Need nonempty to verify
        assert { jarr.nonEmpty }

        jarr.foreach({ json =>
          val m = json.asObject.get.toMap

          assert {
            val arr = m("stops").asArray.get
            arr.nonEmpty && arr.forall({ jobj =>
              Set("description", "numBoard", "numAlight", "index", "minTime", "maxTime")
                .forall(field => jobj.asObject.get.fieldSet contains field)
            })
          }
          assert {
            val arr = m("requests").asArray.get
            arr.nonEmpty && arr.forall({ jobj =>
              Set("start", "end", "time")
                .forall(field => jobj.asObject.get.fieldSet contains field)
            })
          }
        })
      })
      scala.concurrent.Await.result(fut, 60 seconds)
    }
  }

  test("/travel_times returns the travel times") {
    Get("/travel_times/5/10/15/100/150") ~> testService ~> check {
      val travelTimes = _root_.io.circe.parser.parse(responseAs[String])
        .right.get.asArray.get
        .map(_.asNumber.get.toDouble)
        .toList

      val d = (i: Int, j: Int) =>
        testDataSource.distanceFunction(
          testDataSource.busStopsByIndex(i),
          testDataSource.busStopsByIndex(j)
        )

      assert { travelTimes == List(d(5, 10), d(10, 15), d(15, 100), d(100, 150)) }
    }
  }

  test("/paths_requests/x/y/z returns the requests served by x --> y --> z") {
    import sg.beeline.util.kdtreeQuery.squaredDistance

    val servingNth = (n: Int) => {
      val req = getRequests(n)

      (
        testDataSource.busStops.find(stop =>
          squaredDistance(stop.xy, req.start) <= 90000
        ).get.index,
        testDataSource.busStops.find(stop =>
          squaredDistance(stop.xy, req.end) <= 90000
        ).get.index
      )
    }

    val (w, y) = servingNth(100)
    val (x, z) = servingNth(105)

    // w -> y, x -> z
    Get(s"/path_requests/$w/$x/$y/$z?maxDistance=300") ~> testService ~> check {
      val json = _root_.io.circe.parser.parse(responseAs[String]).right.get
      val jarr = json.asArray.get
      val indices = jarr.map(j => j.asObject.get.toMap("id").asNumber.get.toInt.get)
        .toSet

      assert { indices contains 100 }
      assert { indices contains 105 }
    }

    // (wrong direction) y -> w , z -> x
    Get(s"/path_requests/$y/$z/$w/$x?maxDistance=300") ~> testService ~> check {
      val json = _root_.io.circe.parser.parse(responseAs[String]).right.get
      val jarr = json.asArray.get
      val indices = jarr.map(j => j.asObject.get.toMap("id").asNumber.get.toInt.get)
        .toSet

      assert { !(indices contains 100) }
      assert { !(indices contains 105) }
    }
  }

  test("CORS settings work") {
    Options("/bus_stops/1/2/3")
      .addHeader(Origin(HttpOrigin("https://www.beeline.sg")))
      .addHeader(`Access-Control-Request-Headers`())
      .addHeader(`Access-Control-Request-Method`(HttpMethods.GET)) ~>
      testService ~> check {
      val allowOrigin = header[`Access-Control-Allow-Origin`].get
      assert { allowOrigin.range matches HttpOrigin("https://www.beeline.sg") }
    }
  }
}

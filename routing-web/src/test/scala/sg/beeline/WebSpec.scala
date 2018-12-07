package sg.beeline

import java.util.UUID

import akka.http.scaladsl.model.{HttpMethods, StatusCodes, Uri}
import akka.http.scaladsl.testkit.{RouteTestTimeout, ScalatestRouteTest}
import akka.http.scaladsl.server._
import akka.http.scaladsl.model.headers._
import org.scalatest.FunSuite
import sg.beeline.io.{BuiltIn, DataSource, SuggestionsSource}
import sg.beeline.problem.{BusStop, Suggestion}
import sg.beeline.ruinrecreate.{BeelineRecreateSettings, LocalCPUSuggestRouteService}
import sg.beeline.util.{Point, Projections, squaredDistance}
import sg.beeline.web.{E2EAuthSettings, E2ESuggestion, IntelligentRoutingService}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

/**
 * Test that we are returning... at least the expected formats?
 */
class WebSpec extends FunSuite with ScalatestRouteTest {

  implicit val testE2EAuthSettings = new E2EAuthSettings {
    override def googleMapsApiKey: String = ""
    override def authVerificationKey: String = ""
    override def beelineServer: String = ""
  }

  def randomAroundLngLat(p: Point, distance: Double) = {
    val svy = Projections.toSVY(p)

    Projections.toWGS((
      svy._1 + (scala.util.Random.nextDouble() * (2 * distance) - distance),
      svy._2 + (scala.util.Random.nextDouble() * (2 * distance) - distance)
    ))
  }

  val CBD = (103.847246, 1.285621)
  val YEW_TEE = (103.747077, 1.395824)

  // Make a whole bunch of requests
  val suggestionsSource = new SuggestionsSource {
    override val getLiveRequests: Seq[Suggestion] = {
      genericWrapArray(List(8.0, 8.5, 9.0, 9.5)
        .view
        .flatMap({ hour =>
          (0 until 3000).map({ _ =>
            (index: Int) => {
              val randStart = randomAroundLngLat(CBD, 2000) // City
              val randEnd = randomAroundLngLat(YEW_TEE, 2000) // Yew Tee

              Suggestion(index, Projections.toSVY(randStart), Projections.toSVY(randEnd),
                hour * 3600 * 1000L,
                weight = 1,
                createdAt = new java.util.Date(2017, 0, 1, 0, 0).getTime,
                userId = None,
                email=None,
                daysOfWeek = 0x8F,
              )
            }
          })
        })
        .zipWithIndex
        .map({ case (f, i) => f(i) })
        .toArray)
    }

    private def suggestionsFilter(settings: BeelineRecreateSettings, reference: Suggestion): Suggestion => Boolean = {
      s =>
        // Determine whether or not to allow anonymous suggestions
        (settings.includeAnonymous || s.userId.nonEmpty || s.email.nonEmpty) &&
        // Min created time (to ignore the really old requests)
        s.createdAt > settings.createdSince &&
        // Ensure arrival time is plus/minus some value
        (s.time - reference.time).abs <= settings.timeAllowance &&
        // Ensure that the daysOfWeek mask overlaps to some extent
        (!settings.matchDaysOfWeek || (s.daysOfWeek & reference.daysOfWeek) != 0) &&
        squaredDistance(reference.start, s.start) < settings.startClusterRadius * settings.startClusterRadius &&
        squaredDistance(reference.end, s.end) < settings.endClusterRadius * settings.endClusterRadius
    }

    override def similarTo(s: Suggestion, settings: BeelineRecreateSettings): Seq[Suggestion] =
      getLiveRequests.filter(suggestionsFilter(settings, s))
  }

  // FIXME: Because the Lambda ALWAYS takes data from `BuiltIn`
  // It's not possible for us to test with a custom data source
  val testService = new IntelligentRoutingService(
    BuiltIn,
    suggestionsSource,
    LocalCPUSuggestRouteService).myRoute

  // Some assertions on our assumptions
  require { suggestionsSource.getLiveRequests.zipWithIndex.forall { case (o, i) => o.id == i} }

  test("/bus_stops fetches all bus stops") {
    Get("/bus_stops") ~> testService ~> check {
      val jarr = _root_.io.circe.parser.parse(responseAs[String]).right.get

      val currentSetOfBusStops = BuiltIn.busStops.map(_.description).toSet

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
  test ("/routing/begin returns a UUID and polling finally returns a result") {
    import _root_.io.circe.syntax._
    import scala.concurrent.duration._

    implicit val recreateSettingsEncoder = _root_.io.circe.generic
      .semiauto.deriveEncoder[BeelineRecreateSettings]
    implicit val defaultTimeout = RouteTestTimeout(80 seconds)

        // Use a suggestion from the getRequests() --> ensure that at least this one is served
    Get(Uri("/routing/begin").withQuery(Uri.Query(
      "startLat" -> CBD._2.toString,
      "startLng" -> CBD._1.toString,
      "endLat" -> YEW_TEE._2.toString,
      "endLng" -> YEW_TEE._1.toString,
      "time" -> (8.5 * 3600e3).toString,
      "settings" -> _root_.io.circe.Printer.noSpaces.pretty(
        BeelineRecreateSettings(
          maxDetourMinutes = 10.0,
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

      def tryUntilResult(n: Int = 1): Future[String] = {
        Get(Uri("/routing/poll").withQuery(Uri.Query("uuid" -> response))) ~> testService ~>
          check {
            if (status == StatusCodes.Accepted) {
              akka.pattern.after(1 second, using = system.scheduler)({
                tryUntilResult(n + 1)
              })(ExecutionContext.Implicits.global)
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
        assert { jarr.size >= 2 }

        jarr.foreach({ json =>
          val m = json.asObject.get.toMap

          assert { m("stops").asArray.get.size >= 4 }

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

  // TODO: Test for the following
  // - Distance restriction works (all stops are within X m from a request)
  // - Cluster restriction works (all stops are within X m from centre)
  // - Implement time restriction
  test ("/routing/begin returns a UUID and polling finally returns a negative result if error thrown") {
    import _root_.io.circe.syntax._
    import scala.concurrent.duration._

    implicit val recreateSettingsEncoder = _root_.io.circe.generic
      .semiauto.deriveEncoder[BeelineRecreateSettings]
    implicit val defaultTimeout = RouteTestTimeout(80 seconds)

    // Use a suggestion from the getRequests() --> ensure that at least this one is served
    Get(Uri("/routing/begin").withQuery(Uri.Query(
      "startLat" -> CBD._2.toString,
      "startLng" -> CBD._1.toString,
      "endLat" -> YEW_TEE._2.toString,
      "endLng" -> YEW_TEE._1.toString,
      "time" -> (8.5 * 3600e3).toString,
      "settings" -> _root_.io.circe.Printer.noSpaces.pretty(
        BeelineRecreateSettings(
          maxDetourMinutes = 10.0,
          startClusterRadius = 1500,
          startWalkingDistance = 200,
          endClusterRadius = 1500,
          endWalkingDistance = 200,
          minRequests = 100000 // this can never be met
        ).asJson
      ))
    )) ~> testService ~> check {
      val response = responseAs[String]
      val uuidTry = Try { UUID.fromString(response) }

      assert { uuidTry.isSuccess }

      def tryUntilResult(n: Int = 1): Future[Unit] = {
        Get(Uri("/routing/poll").withQuery(Uri.Query("uuid" -> response))) ~> testService ~>
          check {
            if (status == StatusCodes.Accepted) {
              akka.pattern.after(1 second, using = system.scheduler)({
                tryUntilResult(n + 1)
              })(ExecutionContext.Implicits.global)
            } else if (status == StatusCodes.InternalServerError) { // Because minRequests is too high, expect an error
              Future.successful(())
            } else {
              Future.failed(new IllegalStateException(s"Unacceptable status code $status"))
            }
          }
      }

      scala.concurrent.Await.result(tryUntilResult(2), 60 seconds)
    }
  }

  test("/travel_times returns the travel times") {
    Get("/travel_times/5/10/15/100/150") ~> testService ~> check {
      val travelTimes = _root_.io.circe.parser.parse(responseAs[String])
        .right.get.asArray.get
        .map(_.asNumber.get.toDouble)
        .toList

      val d = (i: Int, j: Int) =>
        BuiltIn.distanceFunction(
          BuiltIn.busStopsByIndex(i),
          BuiltIn.busStopsByIndex(j)
        )

      assert { travelTimes == List(d(5, 10), d(10, 15), d(15, 100), d(100, 150)) }
    }
  }

  test("/paths_requests/x/y/z returns the requests served by x --> y --> z") {
    import sg.beeline.util.squaredDistance

    val DIST = 500
    val twoSuggestions = () => {
      scala.util.Random.shuffle(suggestionsSource.getLiveRequests)
        .flatMap { suggestion =>
          val startBusStop = BuiltIn.busStops.find(stop =>
            squaredDistance(stop.xy, suggestion.start) <= DIST * DIST
          )
          val endBusStop = BuiltIn.busStops.find(stop =>
            squaredDistance(stop.xy, suggestion.end) <= DIST * DIST
          )
          for {
            start <- startBusStop
            end <- endBusStop
          } yield (start.index, end.index, suggestion.id)
        }
        .take(2)
    }

    val Seq((w, y, firstRequestIndex), (x, z, secondRequestIndex)) = twoSuggestions()

    // w -> y, x -> z
    Get(s"/path_requests/$w/$x/$y/$z?maxDistance=$DIST") ~> testService ~> check {
      val json = _root_.io.circe.parser.parse(responseAs[String]).right.get
      val jarr = json.asArray.get

      val indices = jarr.map(j => j.asObject.get.toMap("id").asNumber.get.toInt.get)
        .toSet

      assert { indices contains firstRequestIndex }
      assert { indices contains secondRequestIndex }
    }

    // (wrong direction) y -> w , z -> x
    Get(s"/path_requests/$y/$z/$w/$x?maxDistance=$DIST") ~> testService ~> check {
      val json = _root_.io.circe.parser.parse(responseAs[String]).right.get
      val jarr = json.asArray.get
      val indices = jarr.map(j => j.asObject.get.toMap("id").asNumber.get.toInt.get)
        .toSet

      assert { !(indices contains firstRequestIndex) }
      assert { !(indices contains secondRequestIndex) }
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

  test("Optimistic scheduled times") {
    /**
      * First stop time should be the same
      * Second stop should be minimally spaced 1 min from the first stop
      * Third stop should be max of (*)second stop + 1min, and of sched time - 180e3
      * Fourth stop shoudl be max of third stop + 1 min, and of (*)sched time - 180e3
      * Fifth stop should be the same, because it's not a pickup stop
      */
    val list = List(100000, 150000, 300000, 500000, 480000)
    val expected = List(100000, 160000, 220000, 320000, 480000)

    assert { E2ESuggestion.tweakPathTimings(list, 4) == expected }
  }
}

package sg.beeline.web

import java.time.{LocalDate, LocalTime, ZoneId, ZonedDateTime}

import akka.http.scaladsl.HttpExt
import akka.http.scaladsl.model.{HttpRequest, Uri}
import akka.http.scaladsl.unmarshalling.Unmarshal
import akka.stream.Materializer
import io.circe.Json
import sg.beeline.problem.Route2
import sg.beeline.util.Util.Point

import scala.concurrent.{ExecutionContext, Future}

object MapsAPIQuery {
  case class MapsQueryResult(travelTime: Int, encodedPath: Option[String])
}

class MapsAPIQuery(http: HttpExt,
                   materializer: Materializer,
                   executionContext: ExecutionContext) {
  import MapsAPIQuery._
  import sg.beeline.web.JsonMarshallers._
  import BeelineJsonMarshallers._
  import objectJsonMarshallers._

  implicit val ec = executionContext
  implicit val m = materializer

  def getP2PGoogleMapsTravelTime(from: Point, to: Point,
                                 workingDay: LocalDate,
                                 departureTime: Int,
                                 googleMapsApiKey: String): Future[MapsQueryResult] = {
    case class QDuration(value: Int)
    case class QLeg(duration_in_traffic: QDuration)
    case class QPolyline(points: String)
    case class QRoute(legs: List[QLeg], overview_polyline: Option[QPolyline])
    case class QResult(routes: List[QRoute])

    import _root_.io.circe.generic.extras.auto._

    for {
      response <- http.singleRequest(HttpRequest(
        uri=Uri("https://maps.googleapis.com/maps/api/directions/json")
          .withQuery(Uri.Query(
            "origin" -> s"${from._2},${from._1}",
            "destination" -> s"${to._2},${to._1}",
            "mode" -> "driving",
            "traffic_model" -> "best_guess",
            "departure_time" ->
              // Singapore time
              ZonedDateTime.of(workingDay, LocalTime.ofSecondOfDay(departureTime / 1000), ZoneId.of("Asia/Singapore"))
                .toEpochSecond
                .toString,
            "key" -> googleMapsApiKey
          ))
      ))
      qresult <- Unmarshal(response._3).to[Json].map(_.as[QResult].toTry.get)
    } yield {
      // Google API provides times in seconds
      // We convert them to milliseconds for consistency
      MapsQueryResult(
        qresult.routes.head.legs.map(_.duration_in_traffic.value).sum * 1000,
        qresult.routes.head.overview_polyline.map(_.points)
      )
    }
  }
}

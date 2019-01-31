package sg.beeline.web

import java.sql.Timestamp

import io.circe.Decoder.Result
import io.circe.{Decoder, Encoder, HCursor, Json}
import sg.beeline.io.SuggestionsSource.SuggestedRoute
import sg.beeline.problem._
import sg.beeline.util.Projections

object SuggestionJsonEncoder extends Encoder[Suggestion] {
  override def apply(suggestion: Suggestion): Json =
    Json.obj(
      "id" -> Json.fromInt(suggestion.id),
      "start" -> Json.fromFields(RouteJsonEncoder.latLng(Projections.toWGS(suggestion.start))),
      "end" -> Json.fromFields(RouteJsonEncoder.latLng(Projections.toWGS(suggestion.end))),
      "time" -> Json.fromDouble(suggestion.time).get
    )
}

object RequestJsonEncoder extends Encoder[Request] {
  override def apply(request: Request) =
    Json.obj(
      "start" -> Json.fromFields(RouteJsonEncoder.latLng(Projections.toWGS(request.start))),
      "end" -> Json.fromFields(RouteJsonEncoder.latLng(Projections.toWGS(request.end))),
      "time" -> Json.fromDouble(request.time).get
    )
}

object RouteJsonEncoder extends Encoder[Route2] {
  // This case class is only used for serialization
  case class Stop(busStop : BusStop, numBoard : Int, numAlight: Int) {}

  def latLng(d: (Double, Double)) = List(
    "lat" -> Json.fromDouble(d._2).get,
    "lng" -> Json.fromDouble(d._1).get
  )

  override def apply(route: Route2) : Json = {
    import io.circe.syntax._
    implicit val requestEncoder = RequestJsonEncoder

    Json.obj(
      "stops" -> (
        route.pickups.map { case (busStop, requests) => Json.obj(
          "description" -> busStop.description.asJson,
          "lat" -> busStop.coordinates._2.asJson,
          "lng" -> busStop.coordinates._1.asJson,
          "numBoard" -> requests.size.asJson,
          "numAlight" -> 0.asJson,
          "index" -> busStop.index.asJson,
        )} ++
          route.dropoffs.map { case (busStop, requests) => Json.obj(
            "description" -> busStop.description.asJson,
            "lat" -> busStop.coordinates._2.asJson,
            "lng" -> busStop.coordinates._1.asJson,
            "numAlight" -> requests.size.asJson,
            "numBoard" -> 0.asJson,
            "index" -> busStop.index.asJson,
          )}
        )
        .zip(route.times(route.requests.head.time)) // FIXME: save the time on the route...
        .map({ case (json, time) =>
          json.mapObject(o => o.add("maxTime", time.asJson).add("minTime", time.asJson))
        })
        .asJson,
      "requests" -> route.requests.asJson
    )
  }
}

object BusStopEncoder extends Encoder[BusStop] {
  override def apply(a: BusStop): Json = a match {
    case BusStop((x, y), heading, description, roadName, index, stopCode) =>
      Json.obj(
        "coordinates" -> Json.arr(Json.fromDoubleOrNull(x), Json.fromDoubleOrNull(y)),
        "heading" -> Json.fromDoubleOrNull(heading),
        "description" -> Json.fromString(description),
        "roadName" -> Json.fromString(roadName),
        "index" -> Json.fromInt(index),
        "busStopCode" -> a.stopCode.map(Json.fromString).getOrElse(Json.Null)
      )
  }
}

object TimestampEncoder extends Encoder[Timestamp] {
  import java.text.DateFormat
  import java.text.SimpleDateFormat
  import io.circe.syntax._

  val df1 = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSX")

  override def apply(a: Timestamp): Json =
    df1.format(a).asJson
}

object TimestampDecoder extends Decoder[Timestamp] {
  /**
    * Allow an incoming timestamp to either be parsed as a long
    * (milliseconds since epoch) or as a ISO8601 date
    * @param c
    * @return Timestamp
    */
  override def apply(c: HCursor): Result[Timestamp] = {
    val dateAsLong = c.as[Long].right.map(l => new Timestamp(l))
    val dateAsString = c.as[String].right.map(TimestampEncoder.df1.parse)
        .right.map(date => new Timestamp(date.getTime))

    dateAsLong.left.flatMap(_ => dateAsString)
  }
}

object SuggestedRouteEncoder extends Encoder[SuggestedRoute] {
  override def apply(s: SuggestedRoute): Json = {
    import io.circe.syntax.EncoderOps
    Json.obj(
      "id" -> Json.fromInt(s.id),
      "seedSuggestionId" -> Json.fromInt(s.seedSuggestionId),
      "userId" -> s.userId.asJson,
      "routeId" -> s.routeId.asJson,
      "adminEmail" -> s.adminEmail.asJson,
      "route" -> s.route,
      "createdAt" -> Json.fromLong(s.createdAt),
      "updatedAt" -> Json.fromLong(s.updatedAt)
    )
  }
}
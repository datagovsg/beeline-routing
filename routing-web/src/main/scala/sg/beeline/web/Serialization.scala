package sg.beeline.web

import io.circe.{Encoder, Json}
import sg.beeline.problem._
import sg.beeline.util.Util


object SuggestionJsonEncoder extends Encoder[Suggestion] {
  override def apply(suggestion: Suggestion): Json =
    Json.obj(
      "id" -> Json.fromInt(suggestion.id),
      "start" -> Json.fromFields(RouteJsonEncoder.latLng(Util.toWGS(suggestion.start))),
      "end" -> Json.fromFields(RouteJsonEncoder.latLng(Util.toWGS(suggestion.end))),
      "time" -> Json.fromDouble(suggestion.time).get
    )
}

object RequestJsonEncoder extends Encoder[Request] {
  override def apply(request: Request) =
    Json.obj(
      "start" -> Json.fromFields(RouteJsonEncoder.latLng(Util.toWGS(request.start))),
      "end" -> Json.fromFields(RouteJsonEncoder.latLng(Util.toWGS(request.end))),
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
          "numBoard" -> requests.size.asJson,
          "numAlight" -> 0.asJson,
          "index" -> busStop.index.asJson,
        )} ++
          route.dropoffs.map { case (busStop, requests) => Json.obj(
            "description" -> busStop.description.asJson,
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
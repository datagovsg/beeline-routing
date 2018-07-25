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
  override def apply(request: RoutingProblem.Request) =
    Json.obj(
      "start" -> Json.fromFields(RouteJsonEncoder.latLng(Util.toWGS(request.start))),
      "end" -> Json.fromFields(RouteJsonEncoder.latLng(Util.toWGS(request.end))),
      "time" -> Json.fromDouble(request.time).get
    )
}

object RouteJsonEncoder extends Encoder[Route] {
  // This case class is only used for serialization
  case class Stop(busStop : BusStop, numBoard : Int, numAlight: Int) {}

  def latLng(d: (Double, Double)) = List(
    "lat" -> Json.fromDouble(d._2).get,
    "lng" -> Json.fromDouble(d._1).get
  )

  override def apply(route: Route) : Json = {
    val positions = route.activitiesWithTimes.flatMap({
      case (Pickup(r, l), minTime, maxTime) => Some((Stop(l, 1, 0), minTime, maxTime))
      case (Dropoff(r, l), minTime, maxTime) => Some((Stop(l, 0, 1), minTime, maxTime))
      case _ => None
    }).foldRight(
      List[(Stop, Double, Double)]()
    ) { // Remove consecutive runs
      case ((Stop(loc, a, b), minTime, maxTime), Nil) =>
        (Stop(loc, a, b), minTime, maxTime) :: Nil
      case ((Stop(loc1, a1, b1), minTime1, maxTime1), (Stop(loc2, a2, b2), minTime2, maxTime2)::tail) =>
        if (loc1 == loc2)
          (Stop(loc1, a1 + a2, b1 + b2), minTime1, maxTime1) ::tail
        else
          (Stop(loc1, a1, b1), minTime1, maxTime1) :: (Stop(loc2, a2, b2), minTime2, maxTime2) :: tail
    }

    val positionsJson = positions.map({ case (Stop(bs, board, alight), minTime, maxTime) =>
      Json.fromFields(
        latLng(bs.coordinates) ++
          List(
            "description" -> Json.fromString(bs.description),
            "numBoard" -> Json.fromInt(board),
            "numAlight" -> Json.fromInt(alight),
            "index" -> Json.fromInt(bs.index),
            "minTime" -> Json.fromDouble(minTime).get,
            "maxTime" -> Json.fromDouble(maxTime).get
          )
      )
    })

    val requestsJson = route.activities
      .flatMap({ case Pickup(request, loc) => Some(request) case _ => None})
      .map(request => RequestJsonEncoder(request))
      .toList

    Json.obj(
      "stops" -> Json.arr(positionsJson:_*),
      "requests" -> Json.arr(requestsJson:_*)
    )
  }
}

object BusStopEncoder extends Encoder[BusStop] {
  override def apply(a: BusStop): Json = a match {
    case BusStop((x, y), heading, description, roadName, index) =>
      Json.obj(
        "coordinates" -> Json.arr(Json.fromDoubleOrNull(x), Json.fromDoubleOrNull(y)),
        "heading" -> Json.fromDoubleOrNull(heading),
        "description" -> Json.fromString(description),
        "roadName" -> Json.fromString(roadName),
        "index" -> Json.fromInt(index)
      )
  }
}
package sg.beeline
import org.json4s._
import org.json4s.native.JsonMethods._
import scala.io.Source

object Import {
  def getBusStops = {
    implicit val formats = DefaultFormats
    val jsonText = Source.fromFile("bus-stops.json").mkString
    val jsonData = parse(jsonText).asInstanceOf[JArray]

    jsonData.arr
      .filter(v =>
        (v \ "Latitude").extract[Double] != 0.0
      )
      .map(v => new BusStop(
        (
          (v \ "Longitude").extract[Double],
          (v \ "Latitude").extract[Double]
        ),
        (v \ "Description").extract[String],
        (v \ "RoadName").extract[String]
      ))
  }

  // Return the number of seconds since midnight
  def convertTime(timeString: String) =
    timeString.substring(0,2).toInt * 3600000 +
    timeString.substring(2,4).toInt * 60000

  def getRequests = {
    implicit val formats = DefaultFormats
    val jsonText = Source.fromFile("suggestions.json").mkString
    val jsonData = parse(jsonText).asInstanceOf[JArray]

    jsonData.arr
      .filter(_(5).extract[String] != null)
      .map(v => new Suggestion(
        start = Util.toSVY((v(1).extract[Double], v(0).extract[Double])),
        end = Util.toSVY((v(3).extract[Double], v(2).extract[Double])),
        time = convertTime(v(5).extract[String])
      ))
      .filter(_.time == 34200000)
  }
}

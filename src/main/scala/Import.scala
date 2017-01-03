package sg.beeline

import java.io.FileInputStream
import java.util.zip.GZIPInputStream

import org.json4s._
import org.json4s.native.JsonMethods._
import scala.io.Source

object Import {
  lazy val getBusStops = {
    implicit val formats = DefaultFormats
    val jsonText = Source.fromFile("onemap/bus-stops-headings.json").mkString
    val jsonData = parse(jsonText).asInstanceOf[JArray]

    val busStops = jsonData.arr
      .filter(v =>
        (v \ "Latitude").extract[Double] != 0.0
      )
        .zipWithIndex
        .map({
          case (v, i) => new BusStop(
            (
              (v \ "Longitude").extract[Double],
              (v \ "Latitude").extract[Double]
              ),
            (v \ "Heading").extractOrElse[Double](Double.NaN),
            (v \ "Description").extract[String],
            (v \ "RoadName").extract[String],
            i
          )
        })

    busStops
  }

  lazy val distanceMatrix = {
    val ois = new java.io.ObjectInputStream(
      new java.util.zip.GZIPInputStream(
        new java.io.FileInputStream("./distances_cache.dat.gz")))

    ois.readObject().asInstanceOf[Array[Array[Double]]]
  }

  // Return the number of seconds since midnight
  def convertTime(timeString: String) =
    timeString.substring(0,2).toLong * 3600000 +
    timeString.substring(2,4).toLong * 60000

  lazy val getRequests = {
    implicit val formats = DefaultFormats
    val jsonText = Source.fromFile("suggestions.json").mkString
    val jsonData = parse(jsonText).asInstanceOf[JArray]

    jsonData.arr
      .filter(_(5).extract[String] != null)
      .map(v => new Suggestion(
        start = Util.toSVY((v(1).extract[Double], v(0).extract[Double])),
        end = Util.toSVY((v(3).extract[Double], v(2).extract[Double])),
        actualTime = convertTime(v(5).extract[String])
      ))
  }

  lazy val getEzlinkRequests = {
    implicit val formats = DefaultFormats
    val jsonText = new java.util.Scanner(new GZIPInputStream(new FileInputStream("ezlink.json.gz")))
        .useDelimiter("\\Z").next()
    val jsonData = parse(jsonText).asInstanceOf[JArray]

    jsonData.arr
      .filter(_(5).extract[String] != null)
      .map(v => new Suggestion(
        start = Util.toSVY((v(1).extract[Double], v(0).extract[Double])),
        end = Util.toSVY((v(3).extract[Double], v(2).extract[Double])),
        actualTime = convertTime(v(5).extract[String]),
        weight = v(4).extract[Int]
      ))
      .filter(_.weight > 25)
  }
}

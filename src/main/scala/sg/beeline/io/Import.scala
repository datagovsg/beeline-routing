package sg.beeline.io

import java.io.FileInputStream
import java.util.zip.GZIPInputStream

import org.json4s._
import org.json4s.native.JsonMethods._
import sg.beeline.problem.{BusStop, BusStops, MrtStation, Suggestion}
import sg.beeline.util.{ExpiringCache, Util}

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.io.Source


case class BusStopSchema(Latitude: Double, Longitude: Double, Heading: Option[Double], Description: String, RoadName: String)

object Import {

  lazy val getBusStopsOnly = {
    implicit val formats = DefaultFormats + FieldSerializer[BusStopSchema]()
    val jsonText = Source.fromFile("onemap/bus-stops-headings.json").mkString
    val jsonData = parse(jsonText)

    jsonData.extract[Array[BusStopSchema]]
        .zipWithIndex
        .map({
          case (b, i) => BusStop(
            (b.Longitude, b.Latitude),
            b.Heading.getOrElse(Double.NaN),
            b.Description,
            b.RoadName,
            i
          )
        })
  }

  lazy val getBusStops = {
    val busStops = getBusStopsOnly

    val distanceMatrix = {
      val ois = new java.io.ObjectInputStream(
        new java.util.zip.GZIPInputStream(
          new java.io.FileInputStream("./distances_cache.dat.gz")))

      ois.readObject().asInstanceOf[Array[Array[Double]]]
    }

    BusStops(
      busStops,
      (b1: BusStop, b2: BusStop) => distanceMatrix(b1.index)(b2.index)
    )
  }

  lazy val getMrtStations = {
    implicit val formats = DefaultFormats
    val jsonText = Source.fromFile("mrt-stations.json").mkString
    val jsonData = parse(jsonText).asInstanceOf[JArray]

    jsonData.arr
      .zipWithIndex
      .map({
        case(v, i) => new MrtStation(
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
    import org.json4s.native.JsonMethods._

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

  val getLiveRequests : ExpiringCache[Array[Suggestion]] = ExpiringCache(10.minutes) {
    import slick.jdbc.PostgresProfile.api._

    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.duration._

    val (scheme, username, password, host, database) = {
      val URI = new java.net.URI(scala.util.Properties.envOrElse("DATABASE_URL", "(No database URL provided"))
      val userPass = URI.getUserInfo.split(":")

      (
        URI.getScheme,
        userPass(0),
        userPass(1),
        URI.getHost,
        URI.getPath
      )
    }

    val db = Database.forURL(
      s"jdbc:postgresql://${host}${database}?user=${username}&" +
        s"password=${password}&ssl=true&" +
        s"sslfactory=org.postgresql.ssl.NonValidatingFactory",
      driver="org.postgresql.Driver"
    )
    val session = db.createSession()
    val suggestions = sql"""
       |        SELECT
       |            DISTINCT ON (board, alight, time, email)
       |            "travelTime",
       |            id,
       |            ST_X(board) AS board_lng,
       |            ST_Y(board) AS board_lat,
       |            ST_X(alight) AS alight_lng,
       |            ST_Y(alight) AS alight_lat,
       |            email,
       |            time,
       |            "createdAt"
       |        FROM suggestions
       |        ORDER BY board, alight, time, email
       |
       """.stripMargin('|')
          .as[(Long, Int, Double, Double, Double, Double, String, Long, java.sql.Timestamp)]
          .map({ results =>
            results.view.map({
              case (travelTime, id, boardLng, boardLat, alightLng, alightLat, email, time, createdAt) =>
                new Suggestion(
                  start=Util.toSVY((boardLng, boardLat)),
                  end=Util.toSVY((alightLng, alightLat)),
                  actualTime=time
                )
            }).toArray
          })

    Await.result(db.run(suggestions), 60 seconds)
  }

  lazy val getEzlinkRequests = {
    import org.json4s.native.JsonMethods._

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

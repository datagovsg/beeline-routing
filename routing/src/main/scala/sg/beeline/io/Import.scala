package sg.beeline.io

import sg.beeline.problem.{BusStop, BusStops, MrtStation, Suggestion}
import sg.beeline.util.{ExpiringCache, Util}

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.io.Source


case class BusStopSchema(Latitude: Double, Longitude: Double, Heading: Option[Double], Description: String, RoadName: String)

trait DataSource {
  def getBusStopsOnly: Seq[BusStop]
  def getBusStops: BusStops
  def getMrtStations: Seq[MrtStation]
}

object Import extends DataSource {

  override lazy val getBusStopsOnly = {
    implicit val busStopSchemaDecoder = _root_.io.circe.generic
      .semiauto.deriveDecoder[BusStopSchema]

    val jsonData = _root_.io.circe.parser.decode[List[BusStopSchema]](
      Source.fromFile("onemap/bus-stops-headings.json").mkString
    ).right.get

    jsonData
      .zipWithIndex
      .filter(s => s._1.Longitude != 0 && s._1.Latitude != 0)
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

  override lazy val getBusStops = {
    val busStops = getBusStopsOnly

    BusStops(
      busStops,
      (b1, b2) => distanceMatrix(b1.index)(b2.index)
    )
  }

  override lazy val getMrtStations = {
    implicit val busStopSchemaDecoder = _root_.io.circe.generic
      .semiauto.deriveDecoder[BusStopSchema]

    val jsonData = _root_.io.circe.parser.decode[List[BusStopSchema]](
      Source.fromFile("mrt-stations.json").mkString
    ).right.get

    jsonData
      .zipWithIndex
      .map({
        case(v, i) => new MrtStation(
          (v.Longitude, v.Latitude),
          v.Heading.getOrElse(Double.NaN),
          v.Description,
          v.RoadName,
          i
        )
      })
  }

  lazy val distanceMatrix = {
    val ois = new java.io.ObjectInputStream(
      new java.util.zip.GZIPInputStream(
        new java.io.FileInputStream("./distances_cache.dat.gz")))

    /* FIXME Hack: Slow down all timings by 50% to account for peak
      hour bad traffic
     */
    val arr = ois.readObject().asInstanceOf[Array[Array[Double]]]
    arr.foreach { row =>
      row.indices.foreach { i =>
        row(i) = row(i) * 1.5
      }
    }
    arr
  }

  // Return the number of seconds since midnight
  def convertTime(timeString: String) =
    timeString.substring(0,2).toLong * 3600000 +
    timeString.substring(2,4).toLong * 60000

  private val liveRequestsCache : ExpiringCache[Seq[Suggestion]] = ExpiringCache(10.minutes) {
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
          .map[Seq[Suggestion]]({ results =>
            genericWrapArray(results.view.map({
              case (travelTime, id, boardLng, boardLat, alightLng, alightLat, email, time, createdAt) =>
                Suggestion(
                  id=id,
                  start=Util.toSVY((boardLng, boardLat)),
                  end=Util.toSVY((alightLng, alightLat)),
                  actualTime=time
                )
            }).toArray)
          })

    Await.result(db.run(suggestions), 60 seconds)
  }
  def getLiveRequests = liveRequestsCache.apply
}

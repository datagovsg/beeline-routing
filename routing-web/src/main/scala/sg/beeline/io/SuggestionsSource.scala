package sg.beeline.io

import java.sql.Timestamp

import sg.beeline.problem.Suggestion
import sg.beeline.ruinrecreate.BeelineRecreateSettings
import sg.beeline.util.{ExpiringCache, Projections}
import slick.jdbc.SQLActionBuilder

import scala.concurrent.{Await, ExecutionContext}
import scala.concurrent.duration._
import scala.util.Properties
import slick.jdbc.PostgresProfile.api._


class SuggestionsSource {
  private lazy val db = {
    val (username, password, host, database) = {
      val URI = new java.net.URI(
        Properties.propOrElse(
          "database.url",
          Properties.envOrElse("DATABASE_URL", "(No database URL provided)")
        )
      )
      val userPass = URI.getUserInfo.split(":")

      (
        userPass(0),
        userPass(1),
        URI.getHost,
        URI.getPath
      )
    }
    Database.forURL(
      s"jdbc:postgresql://${host}${database}?user=${username}&" +
        s"password=${password}&ssl=true&" +
        s"sslfactory=org.postgresql.ssl.NonValidatingFactory",
      driver = "org.postgresql.Driver"
    )
  }
  // Return the number of seconds since midnight
  def convertTime(timeString: String) =
    timeString.substring(0,2).toLong * 3600000 +
      timeString.substring(2,4).toLong * 60000

  private def timeFn[T](m: String)(t: => T) = {
    val initialTime = System.currentTimeMillis()

    println(m)
    val result = t
    println("Completed: " + m)
    println(m + s" took ${(System.currentTimeMillis() - initialTime) / 1000.0} seconds")
    t
  }

  private def suggestionsFrom(query: SQLActionBuilder)(implicit executionContext: ExecutionContext) = {
    query
      .as[(Long, Int, Double, Double, Double, Double, Option[Int], Option[String], Long, Int, Timestamp)]
      .map[Seq[Suggestion]]({ results =>
      genericWrapArray(results.view.map({
        case (travelTime, id, boardLng, boardLat, alightLng, alightLat, userId, email, time, daysOfWeek, createdAt) =>
          Suggestion(
            id = id,
            start = Projections.toSVY((boardLng, boardLat)),
            end = Projections.toSVY((alightLng, alightLat)),
            time = time,
            createdAt = createdAt.getTime,
            userId = userId,
            daysOfWeek = daysOfWeek,
            email = email
          )
      }).toArray)
    })
  }

  private val liveRequestsCache : ExpiringCache[Seq[Suggestion]] = ExpiringCache(10 minutes) {
    timeFn("Refreshing suggestions cache") {
      import scala.concurrent.ExecutionContext.Implicits.global
      import scala.concurrent.duration._

      val query = sql"""
        SELECT
          DISTINCT ON (board, alight, time, email)
          "travelTime",
          id,
          ST_X(board) AS board_lng,
          ST_Y(board) AS board_lat,
          ST_X(alight) AS alight_lng,
          ST_Y(alight) AS alight_lat,
          "userId",
          email,
          time,
          "daysMask",
          "createdAt"
        FROM suggestions
        ORDER BY board, alight, time, email
      """

      Await.result(db.run(suggestionsFrom(query)), 60 seconds)
    }
  }

  def getLiveRequests: Seq[Suggestion] = liveRequestsCache.apply

  def similarTo(s: Suggestion, settings: BeelineRecreateSettings): Seq[Suggestion] = {
    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.duration._

    val query = sql"""
      SELECT
        DISTINCT ON (board, alight, time, email)
        "travelTime",
        id,
        ST_X(board) AS board_lng,
        ST_Y(board) AS board_lat,
        ST_X(alight) AS alight_lng,
        ST_Y(alight) AS alight_lat,
        "userId",
        email,
        time,
        "daysMask",
        "createdAt"
      FROM suggestions
      WHERE
        (${settings.includeAnonymous} OR "userId" is not null OR email is not null)
        AND extract(epoch from "createdAt") * 1000 > ${settings.createdSince}
        AND ABS(time - ${s.time}) <= ${settings.timeAllowance}
        AND (${!settings.matchDaysOfWeek} OR ("daysMask" & ${s.daysOfWeek}) > 0)
        AND ST_Distance(
          ST_Transform(ST_SetSRID(board, 4326), 3414),
          ST_SetSRID(ST_MakePoint(${s.start._1}, ${s.start._2}), 3414)
        ) < ${settings.startClusterRadius}
        AND ST_Distance(
          ST_Transform(ST_SetSRID(alight, 4326), 3414),
          ST_SetSRID(ST_MakePoint(${s.end._1}, ${s.end._2}), 3414)
        ) < ${settings.endClusterRadius}
      ORDER BY board, alight, time, email
    """

    Await.result(db.run(suggestionsFrom(query)), 60 seconds)
  }
}

object SuggestionsSource {
  val DEFAULT = new SuggestionsSource
}
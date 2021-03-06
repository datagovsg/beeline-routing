package sg.beeline.io

import java.sql.Timestamp

import io.circe.Json
import sg.beeline.io.SuggestionsSource.{SuggestedRoute, SuggestedRouteTuple}
import sg.beeline.problem.Suggestion
import sg.beeline.ruinrecreate.BeelineRecreateSettings
import sg.beeline.util.{ExpiringCache, Projections}
import slick.jdbc.SQLActionBuilder

import scala.concurrent.{Await, ExecutionContext, Future}
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
      .as[(Long, Int, Double, Double, Double, Double, Option[Int], Option[String], Long, Int, Timestamp, Option[Timestamp])]
      .map[Seq[Suggestion]]({ results =>
      genericWrapArray(results.view.map({
        case (travelTime, id, boardLng, boardLat, alightLng, alightLat, userId, email, time, daysOfWeek, createdAt, lastTriggerTime) =>
          Suggestion(
            id = id,
            start = Projections.toSVY((boardLng, boardLat)),
            end = Projections.toSVY((alightLng, alightLat)),
            time = time,
            createdAt = createdAt.getTime,
            userId = userId,
            daysOfWeek = daysOfWeek,
            email = email,
            lastTriggerMillis = lastTriggerTime.map(_.getTime)
          )
      }).toArray)
    })
  }

  private val liveRequestsCache : ExpiringCache[Seq[Suggestion]] = ExpiringCache(10 minutes) {
    timeFn("Refreshing suggestions cache") {
      import scala.concurrent.ExecutionContext.Implicits.global

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
          "createdAt",
          "lastTriggerTime"
        FROM suggestions
        ORDER BY board, alight, time, email
      """

      Await.result(db.run(suggestionsFrom(query)), 60 seconds)
    }
  }

  def getLiveRequests: Seq[Suggestion] = liveRequestsCache.apply

  def similarTo(s: Suggestion, settings: BeelineRecreateSettings): Seq[Suggestion] = {
    import scala.concurrent.ExecutionContext.Implicits.global

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
        "createdAt",
        "lastTriggerTime"
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

  def byId(id: Int): Option[Suggestion] = {
    import scala.concurrent.ExecutionContext.Implicits.global
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
        "createdAt",
        "lastTriggerTime"
      FROM suggestions
      WHERE
        id = $id
    """

    Await.result(db.run(suggestionsFrom(query)), 60 seconds).headOption
  }

  def markTriggerTimestamp(suggestionId: Int): Unit = {
    val statement = sqlu"""
      UPDATE suggestions
      SET "lastTriggerTime" = NOW()
      WHERE id = $suggestionId
    """

    Await.result(db.run(statement), 60 seconds)
  }

  def insertSuggestedRoute(seedSuggestionId: Int, route: Json): Future[Int] = {
    val statement = sqlu"""
      INSERT INTO "suggestedRoutes" ("seedSuggestionId", route, "adminEmail", "createdAt", "updatedAt")
      VALUES ($seedSuggestionId, CAST(${route.noSpaces} AS jsonb), 'routing@beeline.sg', NOW(), NOW())
    """
    db.run(statement)
  }

  def getSuggestedRoutes(suggestionId: Int)(implicit executionContext: ExecutionContext): Seq[SuggestedRoute] = {
    val query = sql"""
      SELECT "id", "seedSuggestionId", "userId", "routeId", "adminEmail", "route", "createdAt", "updatedAt"
      FROM "suggestedRoutes"
      WHERE "seedSuggestionId" = $suggestionId
      ORDER BY "id" DESC
    """
    val queryThenMap = query
      .as[SuggestedRouteTuple]
      .map(_.view.map(s => new SuggestedRoute(s)))
    Await.result(db.run(queryThenMap), 60 seconds)
  }

}

object SuggestionsSource {
  val DEFAULT = new SuggestionsSource

  type SuggestedRouteTuple = (Int, Int, Option[Int], Option[Int], Option[String], String, Timestamp, Timestamp)

  case class SuggestedRoute(id: Int, seedSuggestionId: Int, userId: Option[Int], routeId: Option[Int], adminEmail: Option[String], route: Json, createdAt: Long, updatedAt: Long) {
    def this(s: SuggestedRouteTuple) {
      this(s._1, s._2, s._3, s._4, s._5, io.circe.parser.parse(s._6).right.get, s._7.getTime, s._8.getTime)
    }
  }
}
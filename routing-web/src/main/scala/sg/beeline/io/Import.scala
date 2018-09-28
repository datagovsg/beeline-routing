package sg.beeline.io

import sg.beeline.problem.{BusStop, Suggestion}
import sg.beeline.util.{ExpiringCache, Util}

import scala.concurrent.Await
import scala.concurrent.duration._

object Import {
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


  private val liveRequestsCache : ExpiringCache[Seq[Suggestion]] = ExpiringCache(10 minutes) {
    timeFn("Refreshing suggestions cache") {
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
        driver = "org.postgresql.Driver"
      )
      val session = db.createSession()
      val suggestions = sql"""
         SELECT
             DISTINCT ON (board, alight, time, email)
             "travelTime",
             id,
             ST_X(board) AS board_lng,
             ST_Y(board) AS board_lat,
             ST_X(alight) AS alight_lng,
             ST_Y(alight) AS alight_lat,
             userId,
             email,
             time,
             "daysMask",
             "createdAt"
         FROM suggestions
         ORDER BY board, alight, time, email
       """
        .as[(Long, Int, Double, Double, Double, Double, Option[Int], String, Long, Int, java.sql.Timestamp)]
        .map[Seq[Suggestion]]({ results =>
        genericWrapArray(results.view.map({
          case (travelTime, id, boardLng, boardLat, alightLng, alightLat, userId, email, time, daysOfWeek, createdAt) =>
            Suggestion(
              id = id,
              start = Util.toSVY((boardLng, boardLat)),
              end = Util.toSVY((alightLng, alightLat)),
              time = time,
              createdAt = createdAt.getTime,
              userId = userId,
              daysOfWeek = daysOfWeek
            )
        }).toArray)
      })

      Await.result(db.run(suggestions), 60 seconds)
    }
  }
  def getLiveRequests = liveRequestsCache.apply
}

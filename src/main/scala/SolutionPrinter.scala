package sg.beeline
import org.json4s._
import org.json4s.native.JsonMethods._
import org.json4s.JsonDSL._

import java.io._

object SolutionPrinter {

  def writeProblem(requests: Seq[Request]) = {
    val pw = new PrintWriter(new File("static/problem.json"))

    val json = requests.map(req => {
      val startWGS = Util.toWGS(req.start)
      val endWGS = Util.toWGS(req.end)

      ("start" ->
        ("lat" -> startWGS._2) ~
        ("lng" -> startWGS._1)
      ) ~
      ("end" ->
        ("lat" -> endWGS._2) ~
        ("lng" -> endWGS._1)
      )
    })

    pw.write(pretty(render(json)))
    pw.close()
  }

  def writeLocations(busStops: Seq[BusStop]) = {
    val pw = new PrintWriter(new File("static/busStops.json"))

    val json = busStops.map(bs =>
      ("lat" -> bs.coordinates._2) ~
      ("lng" -> bs.coordinates._1) ~
      ("Description" -> bs.description)
    )

    pw.write(pretty(render(json)))
    pw.close()
  }

  def writeSolution(routes: Seq[Route]) = {
    val pw = new PrintWriter(new File("static/routes.json"))
    val json = routes.map(route => {
      ("path" -> route.activities.map(_.location).flatten.map(loc => {
        ("lat" -> loc.coordinates._2) ~
        ("lng" -> loc.coordinates._1)
      }).toList) ~
      ("count" -> (route.activities.size / 2 - 1))
    }).toList

    pw.write(pretty(render(json)))
    pw.close()
  }

}

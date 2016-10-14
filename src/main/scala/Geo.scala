package sg.beeline
import io.jeo.proj.Proj
import com.graphhopper.GHRequest
import com.graphhopper.GHResponse
import com.graphhopper.GraphHopper
import com.graphhopper.PathWrapper
import com.graphhopper.util.shapes.GHPoint
import com.graphhopper.util.{Parameters, CmdArgs}

import scala.collection.mutable.ArrayBuffer

object Geo {
  import Util.Point

  // set up graphhopper
  var graphHopper : GraphHopper = null;
  def initialize() = {
    if (graphHopper == null) {
      graphHopper = new GraphHopper();
      graphHopper.setOSMFile("SG.pbf");
      graphHopper.init(CmdArgs.readFromConfig("config.properties", "graphhopper.config"));
      graphHopper.setCHEnabled(false);
      // graphHopper.setGraphHopperLocation("/home/daniel/intelligent-routing/gh");
      graphHopper.importOrLoad();
    }
  }

  def routeWithJitter(a: (Double, Double), ah: Double, b: (Double, Double), bh: Double) = {
    initialize()
    // Because sometimes routing fails, we perturb the locations
    // by some increasing amount until the routing succeeds
    val fuzzAmounts = List(0.0) // +: (for (i <- List(0.0005, 0.001, 0.002, 0.004);
                                //   j <- 0 until 50) yield i)

    require(ah.isNaN() || 0.0 <= ah && ah <= 360)
    require(bh.isNaN() || 0.0 <= bh && bh <= 360)

    def routeWithJitter(range : Double) = {
      val pa = new GHPoint(
        a._2 - range + 2 * range * Math.random,
        a._1 - range + 2 * range * Math.random
      )
      val pb = new GHPoint(
        b._2 - range + 2 * range * Math.random,
        b._1 - range + 2 * range * Math.random
      )
      val request = new GHRequest(pa, pb, ah, bh)
      request.getHints().put(Parameters.Routing.PASS_THROUGH, true)
      graphHopper.route(request)
    }

    fuzzAmounts.iterator
      .map(routeWithJitter)
      .find(!_.hasErrors)
  }

  def travelTime(a: (Double, Double), ah: Double, b: (Double, Double), bh: Double) : Double = {
    val bestRoute = routeWithJitter(a, ah, b, bh)

    bestRoute match {
      case None => Double.PositiveInfinity
      case Some(ghResponse) =>
        // It is also possible for us to get the routed distance
        ghResponse.getBest.getTime
    }
  }

  def travelPath(a: (Double, Double), ah: Double, b: (Double, Double), bh: Double) = {
    val bestRoute = routeWithJitter(a, ah, b, bh)

    bestRoute match {
      case None => List()
      case Some(ghResponse) =>
        var path = ghResponse.getBest.getPoints
        val arr = new ArrayBuffer[(Double, Double)]

        for (i <- 0 until path.size) {
          arr += ((path.getLon(i), path.getLat(i)))
        }

        arr.toSeq
    }
  }
}

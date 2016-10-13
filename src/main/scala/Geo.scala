package sg.beeline
import io.jeo.proj.Proj
import com.graphhopper.GHRequest
import com.graphhopper.GHResponse
import com.graphhopper.GraphHopper
import com.graphhopper.PathWrapper
import com.graphhopper.util.shapes.GHPoint
import com.graphhopper.util.CmdArgs

import scala.collection.mutable.ArrayBuffer

object Geo {
  import Util.Point

  // set up graphhopper
  var graphHopper : GraphHopper = null;
  def initialize() = {
    if (graphHopper == null) {
      graphHopper = new GraphHopper();
      graphHopper.setOSMFile("SG.pbf");
      graphHopper.init(new CmdArgs());
      // graphHopper.setGraphHopperLocation("/home/daniel/intelligent-routing/gh");
      graphHopper.importOrLoad();
    }
  }

  def routeWithJitter(a: (Double, Double), b: (Double, Double)) = {
    initialize()
    // pick two points
    val request = new GHRequest()
      .addPoint(new GHPoint(a._2, a._1))
      .addPoint(new GHPoint(b._2, b._1));

    // route
    // val response = graphHopper.route(request);

    // Because sometimes routing fails, we perturb the locations
    // by some increasing amount until the routing succeeds
    val fuzzAmounts = List(0.0) // +: (for (i <- List(0.0005, 0.001, 0.002, 0.004);
                                //   j <- 0 until 50) yield i)

    def routeWithJitter(range : Double) = {
      val p1 = new GHPoint(
        a._2 - range + 2 * range * Math.random,
        a._1 - range + 2 * range * Math.random
      )
      val p2 = new GHPoint(
        b._2 - range + 2 * range * Math.random,
        b._1 - range + 2 * range * Math.random
      )
      graphHopper.route(new GHRequest(p1, p2))
    }

    fuzzAmounts.iterator
      .map(routeWithJitter)
      .find(!_.hasErrors)
  }

  def travelTime(a: (Double, Double), b: (Double, Double)) : Double = {
    val bestRoute = routeWithJitter(a,b)

    bestRoute match {
      case None => Double.PositiveInfinity
      case Some(ghResponse) =>
        // It is also possible for us to get the routed distance
        ghResponse.getBest.getTime
    }
  }

  def travelPath(a: (Double, Double), b: (Double, Double)) = {
    val bestRoute = routeWithJitter(a,b)

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

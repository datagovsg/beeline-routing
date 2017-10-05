package sg.beeline.util

import com.graphhopper.util.shapes.GHPoint
import com.graphhopper.util.{CmdArgs, Parameters}
import com.graphhopper.{GHRequest, GraphHopper}

import scala.collection.mutable.ArrayBuffer
;

object Geo {
  implicit class InstructionListWrapper(val instructions : com.graphhopper.util.InstructionList) {
    def list = {
      (for (i <- 0 until instructions.size) yield instructions.get(i)).toList
    }
  }

  // set up graphhopper
  var graphHopper : GraphHopper = null;

  def turnCosts(sign: Int) = (sign match {
    case com.graphhopper.util.Instruction.TURN_LEFT => 5
    case com.graphhopper.util.Instruction.TURN_SHARP_LEFT => 5
    case com.graphhopper.util.Instruction.TURN_RIGHT => 30
    case com.graphhopper.util.Instruction.TURN_SHARP_RIGHT => 20
    case _ => 0
  }) * 1000.0

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
      request.getHints()
        .put(Parameters.Routing.PASS_THROUGH, true)
        .put(Parameters.Routing.HEADING_PENALTY, 3600 * 10) /* 10 hours -- that ought to be enough to force the heading everywhere? */
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

  /* Compute the heading of latLng2 from latLng1 -- equirectangular approximation */
  def heading(latLng1 : (Double, Double), latLng2 : (Double, Double)) = {
    val degreesToRadians = Math.PI / 180

    val x = (latLng2._2 - latLng1._2) * degreesToRadians * Math.cos((latLng2._1 + latLng1._1) * degreesToRadians / 2);
    val y = (latLng2._1 - latLng1._1) * degreesToRadians;

    (x, y)
  }

  def penalizedTravelTime(a: (Double, Double), ah: Double, b: (Double, Double), bh: Double) : Double = {
    val bestRoute = routeWithJitter(a, ah, b, bh)

    val a_heading = (Math.sin(ah / 180 * Math.PI), Math.cos(ah / 180 * Math.PI))
    val b_heading = (Math.sin(bh / 180 * Math.PI), Math.cos(bh / 180 * Math.PI))

    def dot(a : (Double, Double), b : (Double, Double)) = (a, b) match {
      case ((x1, y1), (x2, y2)) => (x2 * x1 + y2 * y1) /
        Math.sqrt( x1*x1 + y1*y1 ) /
        Math.sqrt( x2*x2 + y2*y2 )
    }

    bestRoute match {
      case None => Double.PositiveInfinity
      case Some(ghResponse) =>
        val best = ghResponse.getBest
        val points = best.getPoints

        if (points.size < 2)
          best.getInstructions.list.map({
            instr => turnCosts(instr.getSign) + instr.getTime
          }).sum
        else {
          /*
          Penalize the travel time if heading is wrong despite our best efforts.
          Happens if our origin and destination are on the same road segment.
          Since we penalize by up to 10 mins (very jialat for just being down the road!),
          this should be sufficient to discourage
          erroneous detours
           */
          val a_h1 = (points.getLatitude(0), points.getLongitude(0))
          val a_h2 = (points.getLatitude(1), points.getLongitude(1))
          val a_routed_heading = heading(a_h1, a_h2)

          val b_h1 = (points.getLatitude(points.size - 2), points.getLongitude(points.size - 2))
          val b_h2 = (points.getLatitude(points.size - 1), points.getLongitude(points.size - 1))
          val b_routed_heading = heading(b_h1, b_h2)

          best.getInstructions.list.map({
            instr => turnCosts(instr.getSign) + instr.getTime
          }).sum +
            (if (dot(a_routed_heading, a_heading) < 0.5 ||
                 dot(b_routed_heading, b_heading) < 0.5) 10 * 60000 else 0)
        }
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

  def travelPathStr(a: (Double, Double), ah: Double, b: (Double, Double), bh: Double) = {
    "[" + (travelPath(a, ah, b, bh) map {case (a,b) => s"[${b}, ${a}]"} mkString ",") + "]"
  }
}

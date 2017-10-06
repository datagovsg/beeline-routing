package sg.beeline.clustering

import java.util.Scanner

import scala.annotation.tailrec

/**
  * This program reads a list of points from stdin, and outputs
  * a list of cluster centres and the number of people the cluster centre
  * will pick up, excluding the people who have already been included in
  * earlier clusters.
  *
  * This is an approximate algorithm (for speed). Points are aligned to a grid
  * with spacing about 1/10th of the clustering distance. Therefore their
  * position can be off by sqrt(2) * 0.05 * CLUSTERING_DISTANCE
  *
  * Input:
  * <space> refers to any whitespace
  *
  * NUM_CLUSTERS <space> CLUSTERING_DISTANCE (metres) <space>
  *   POINT_0_X <space> POINT_0_Y <space>
  *   POINT_1_X <space> POINT_1_Y <space>
  *   POINT_2_X <space> POINT_2_Y <space>
  *   ... etc
  *   POINT_N_X <space> POINT_N_Y <space>
  *
  * Output: (the i-th cluster centre is the index_i-th point)
  *
  * X_0 Y_0 IMPROVEMENT_0
  * X_1 Y_1 IMPROVEMENT_1
  * X_2 Y_2 IMPROVEMENT_2
  * X_3 Y_3 IMPROVEMENT_3
  *
  */

object FuzzyDistanceClusterApp {
  def main(args : Array[String]) {
    // Read the input...
    val (distance, requests, weights) = {
      val inp = new Scanner(System.in)
      val distance = inp.nextInt()
      val numEntries = inp.nextInt()
      val locationsArray = scala.collection.mutable.ArrayBuilder.make[(Double, Double)]
      val weightsArray = scala.collection.mutable.ArrayBuilder.make[Int]

      while (inp.hasNext()) {
        locationsArray += ((inp.nextDouble(), inp.nextDouble()))
        weightsArray += inp.nextInt
      }
      (distance, locationsArray.result(), weightsArray.result())
    }

    val requestsIndexWeight = (requests.zipWithIndex, weights).zipped.map({
      case ((xy, index), weight) => (xy, (weight, index))
    })

    val clusters = (new FuzzyDistanceCluster(distance, distance * 0.09))
      .apply(requestsIndexWeight)

    clusters.toSeq.map({
      case (xy, items) => (xy, items.map(_._1).size)
    }).sortBy(_._2)
      .foreach {
        case ((x, y), weight) => println(s"$x $y $weight")
      }
}
}

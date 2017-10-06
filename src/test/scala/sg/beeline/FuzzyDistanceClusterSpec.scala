package sg.beeline

import org.scalatest.FunSuite
import sg.beeline.clustering.FuzzyDistanceCluster

class FuzzyDistanceClusterSpec extends FunSuite {
  test("Fuzzy distance cluster") {
    val clusterFun = new FuzzyDistanceCluster[String](10, 1)

    val results = clusterFun(List(
      ((10.1, 9.9), "Alvin"),
      ((17.8, 10.0), "Benny"),
      ((10.2, 17.8), "Carlos"),
      ((32.1, 13.2), "Dick")
    ))

    assert {
      results((32, 13)).map(s => s._2) == List("Dick")
    }

    val possibleCentres = List(
      results.get((10.0, 10.0)),
      results.get((18.0, 10.0)),
      results.get((10.0, 18.0))
    )

    assert {possibleCentres.count(_.nonEmpty) == 1}
    assert {possibleCentres.flatten.flatten.map(_._2).sorted ==
              List("Alvin", "Benny", "Carlos")}
  }
}

package sg.beeline

import org.scalatest._
import sg.beeline.clustering.FuzzyPointMap

class FuzzyPointMapSpec extends FunSuite {

  implicit class ApproxPoint(p1: (Double, Double)) {
    def about001(p2: (Double, Double)): Boolean =
      p2._1 - 0.001 < p1._1 && p1._1 < p2._1 + 0.001 &&
      p2._2 - 0.001 < p1._2 && p1._2 < p2._2 + 0.001
  }

  test("Key functions") {
    assert {
      FuzzyPointMap.key(0.2)(0.11, 0.29) == (1, 1)
    }

    assert {
      FuzzyPointMap.key(0.2)(0.31, 0.29) == (2, 1)
    }
  }

  test("Reverse key functions") {
    assert {
      FuzzyPointMap.gridCenter(0.2)(1, 5) about001 (0.2, 1.0)
    }
  }

  test("FuzzyWeightPointMap preserves points") {
    import scala.util.Random

    val allMyPoints = (0 until 10000)
        .map(_ => (
          (Random.nextDouble(), Random.nextDouble()),
          Random.nextInt(1000000) // Random payload
        ))

    val gridSize = 0.1
    val mapped = FuzzyPointMap.fromSeq(allMyPoints, gridSize)

    mapped.map.foreach {
      case ((gx, gy), set) =>
        set.foreach {
          case ((x, y), p) =>
            assert {FuzzyPointMap.key(gridSize)((x, y)) == (gx, gy)}
            assert {
              val (gcx, gcy) = FuzzyPointMap.gridCenter(gridSize)(gx, gy)
              Math.abs(gcx - x) <= gridSize / 2 &&
              Math.abs(gcy - y) <= gridSize / 2
            }
        }
    }
  }
}

package sg.beeline

import org.scalatest._
import sg.beeline.util.ListOps

import scala.util.Random

class UtilSpec extends FunSuite with Matchers {

  test("groupSuccessive (1)") {
    val numbers = for (i <- 0 until 100) yield Random.nextInt(10)

    val groupBy2 = new ListOps(numbers).groupSuccessive(_ % 2)

    val regenerated = groupBy2.flatMap({case (_, ls) => ls})

    regenerated.length should be (numbers.length)
    regenerated.zip(numbers).forall({case (a,b) => a == b}) should be (true)

    groupBy2.forall({
      case (rem, ls) =>
        ls.forall(_ % 2 == rem)
    }) should be (true)

    groupBy2.sliding(2).forall({
      case Seq((rem, ls), (rem2, ls2)) =>
        rem != rem2
    }) should be (true)
  }

  test("groupSuccessive (2)") {
    val numbers = 0 until 50

    val grouped = new ListOps(numbers).groupSuccessive(_ / 5)
      .map(_._2)

    assert {
      grouped == List(
        (0 until 5).toList,
        (5 until 10).toList,
        (10 until 15).toList,
        (15 until 20).toList,
        (20 until 25).toList,
        (25 until 30).toList,
        (30 until 35).toList,
        (35 until 40).toList,
        (40 until 45).toList,
        (45 until 50).toList
      )
    }
  }
}

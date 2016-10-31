import org.scalatest._
import sg.beeline.Route
import scala.util.Random

class UtilSpec extends FlatSpec with Matchers {

  "groupSuccessive" should "function correctly" in {
    val numbers = for (i <- 0 until 100) yield Random.nextInt(10)

    val groupBy2 = Route.groupSuccessive(numbers)(_ % 2)

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
//
//  "distanceCache" should "obey triangle inequality" in {
//    val m = sg.beeline.Import.distanceMatrix
//
//
//  }

}

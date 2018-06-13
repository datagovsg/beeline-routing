package sg.beeline

import org.scalatest.FunSuite
import sg.beeline.util.WeightedRandomShuffle

class WeightedRandomShuffleSpec extends FunSuite {

  test("Weighted random shuffle puts highly weighted objects first") {
    val N = 10000
    val list = (0 until N).toArray

    val count = (0 until 1000).count({ _ =>
      val priorityObject = scala.util.Random.nextInt(N)

      val result = WeightedRandomShuffle.shuffle(
        list.map(Integer.valueOf),
        Array.tabulate(N)(i => if (i == priorityObject) N - 1.0 else 1.0)
      )

      result.head == priorityObject
    })
    assert { count > 450 && count < 550 }
  }
}

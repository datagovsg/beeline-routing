package sg.beeline

import org.scalatest.FunSuite

class ListInsertionPointOpsInsertedSubsequenceSpec extends FunSuite {
  val basicList = List(1, 2, 3, 4, 5, 6)

  test ("inserts into identical IPs correctly") {
    assert {
      new util.ListInsertionPointOps[Int](basicList)
        .insertedSubsequence(6, 7, (2, 3), (2, 3)) == List(2, 6, 7, 3)
    }
  }

  test ("inserts into adjacent IPs correctly") {
    assert {
      new util.ListInsertionPointOps[Int](basicList)
        .insertedSubsequence(6, 7, (2, 3), (3, 4)) == List(2, 6, 3, 7, 4)
    }

    assert {
      new util.ListInsertionPointOps[Int](basicList)
        .insertedSubsequence(6, 7, (2, 3), (5, 6)) == List(2, 6, 3, 4, 5, 7, 6)
    }
  }

  test ("inserts into non-adjacent IPs correctly") {
    assert {
      new util.ListInsertionPointOps[Int](basicList)
        .insertedSubsequence(6, 7, (2, 3), (4, 5)) == List(2, 6, 3, 4, 7, 5)
    }
  }

  test ("throws when first insertion point not found") {
    assertThrows[IllegalArgumentException] {
      new util.ListInsertionPointOps[Int](basicList)
        .insertedSubsequence(6, 7, (2, 4), (4, 5))
    }
  }
  test ("throws when second insertion point not found") {
    assertThrows[IllegalArgumentException] {
      new util.ListInsertionPointOps[Int](basicList)
        .insertedSubsequence(6, 7, (2, 3), (2, 4))
    }
  }
  test ("throws when identical insertion points not found") {
    assertThrows[IllegalArgumentException] {
      new util.ListInsertionPointOps[Int](basicList)
        .insertedSubsequence(6, 7, (2, 4), (2, 4))
    }
  }
}

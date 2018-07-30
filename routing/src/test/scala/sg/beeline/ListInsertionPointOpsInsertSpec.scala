package sg.beeline

import org.scalatest.FunSuite

class ListInsertionPointOpsInsertSpec extends FunSuite {
  val basicList = List(1, 2, 3, 4, 5, 6)

  test ("inserts into identical IPs correctly") {
    assert {
      new util.ListInsertionPointOps[Int](basicList)
        .insert(6, 7, (2, 3), (2, 3)) == List(1, 2, 6, 7, 3, 4, 5, 6)
    }
  }

  test ("inserts into adjacent IPs correctly") {
    assert {
      new util.ListInsertionPointOps[Int](basicList)
        .insert(6, 7, (2, 3), (3, 4)) == List(1, 2, 6, 3, 7, 4, 5, 6)
    }

    assert {
      new util.ListInsertionPointOps[Int](basicList)
        .insert(6, 7, (2, 3), (5, 6)) == List(1, 2, 6, 3, 4, 5, 7, 6)
    }
  }

  test ("inserts into non-adjacent IPs correctly") {
    assert {
      new util.ListInsertionPointOps[Int](basicList)
        .insert(6, 7, (2, 3), (4, 5)) == List(1, 2, 6, 3, 4, 7, 5, 6)
    }
  }

  test ("throws when second insertion point not found") {
    assertThrows[IllegalArgumentException] {
      new util.ListInsertionPointOps[Int](basicList)
        .insert(6, 7, (2, 3), (4, 6))
    }
  }
  test ("throws when first insertion point not found") {
    assertThrows[IllegalArgumentException] {
      new util.ListInsertionPointOps[Int](basicList)
        .insert(6, 7, (2, 4), (2, 3))
    }
  }
  test ("throws when identical insertion points not found") {
    assertThrows[IllegalArgumentException] {
      new util.ListInsertionPointOps[Int](basicList)
        .insert(6, 7, (2, 4), (2, 4))
    }
  }
}

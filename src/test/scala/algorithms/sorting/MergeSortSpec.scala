package algorithms.sorting

import org.scalatest.FunSuite

class MergeSortSpec extends FunSuite {

  test("sort") {
    assert(MergeSort.sort(Seq(3, 6, 1, 5, 2, 0, 8)) == Seq(0, 1, 2, 3, 5, 6, 8))
  }

}

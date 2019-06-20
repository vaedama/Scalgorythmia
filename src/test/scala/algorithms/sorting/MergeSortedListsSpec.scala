package algorithms.sorting

import org.scalatest.FunSuite

class MergeSortedListsSpec extends FunSuite {

  test("merge test 1") {
    assert(MergeSortedLists.merge(Seq(1, 2, 4), Seq(1, 3, 4)) == Seq(1, 1, 2, 3, 4, 4))
  }

  test("merge test 2") {
    assert(MergeSortedLists.merge(Seq(1, 2, 4), Seq(3, 4)) == Seq(1, 2, 3, 4, 4))
  }

  test("mergeN test 1") {
    assert(MergeSortedLists.mergeN(List(List(1, 4, 5), List(1, 3, 4), List(2, 6))) == List(1, 1, 2, 3, 4, 4, 5, 6))
  }

  test("mergeN test 2") {
    assert(MergeSortedLists.mergeN(List(List(1, 4, 5), List(1), List(2, 6, 8))) == List(1, 1, 2, 4, 5, 6, 8))
  }

}

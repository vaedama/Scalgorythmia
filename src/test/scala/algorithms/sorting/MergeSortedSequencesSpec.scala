package algorithms.sorting

import org.scalatest.FunSuite

class MergeSortedSequencesSpec extends FunSuite {

  test("merge test 1") {
    assert(MergeSortedSequences.merge(Seq(1, 2, 4), Seq(1, 3, 4)) === Seq(1, 1, 2, 3, 4, 4))
  }

  test("merge test 2") {
    assert(MergeSortedSequences.merge(Seq(1, 2, 4), Seq(3, 4)) === Seq(1, 2, 3, 4, 4))
  }

  test("mergeN test 1") {
    assert(MergeSortedSequences.mergeN(Seq(Seq(1, 4, 5), Seq(1, 3, 4), Seq(2, 6))) === Seq(1, 1, 2, 3, 4, 4, 5, 6))
  }

  test("mergeN test 2") {
    assert(MergeSortedSequences.mergeN(Seq(Seq(1, 4, 5), Seq(1), Seq(2, 6, 8))) === Seq(1, 1, 2, 4, 5, 6, 8))
  }

}

package algorithms.merge_intervals

import org.scalatest.FunSuite

class MergeOverlappingIntervalsSpec extends FunSuite {

  test("case 1") {
    val actual = MergeOverlappingIntervals.merge(Seq((1, 5), (3, 7), (4, 6), (6, 8)))
    val expected = Seq((1, 8))
    assert(expected == actual)
  }

  test("case 2") {
    val actual = MergeOverlappingIntervals.merge(Seq((10, 12), (12, 15)))
    val expected = Seq((10, 15))
    assert(expected == actual)
  }

}

package algorithms.sorting

import org.scalatest.FunSuite

class MedianOfTwoSortedArraysSpec extends FunSuite {

  test("case 1") {
    assert(MedianOfTwoSortedArrays.median(Array(1, 3), Array(2)) === 2)
  }

  test("case 2") {
    assert(MedianOfTwoSortedArrays.median(Array(1, 2), Array(3, 4)) === 2.5)
  }

}

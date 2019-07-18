package algorithms.searching

import org.scalatest.FunSuite

class BinarySearchSpec extends FunSuite {

  private val array = Array(1, 10, 20, 47, 59, 63, 75, 88, 99, 107, 120, 133, 155, 162, 176, 188, 199, 200, 210, 222)

  test("indexOf 47") {
    assert(BinarySearch.liftIndex(47, array).contains(3))
  }

  test("indexOf 200") {
    assert(BinarySearch.liftIndex(200, array).contains(17))
  }

  test("indexOf 201") {
    assert(BinarySearch.liftIndex(201, array).isEmpty)
  }

  test("indexOfInRotatedArray") {
    val array = Array(176, 188, 199, 200, 210, 222, 1, 10, 20, 47, 59, 63, 75, 88, 99, 107, 120, 133, 155, 162)
    assert(BinarySearch.liftIndexInRotatedArray(200, array).contains(3))
  }

}

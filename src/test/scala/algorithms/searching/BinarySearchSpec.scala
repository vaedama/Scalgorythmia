package algorithms.searching

import org.scalatest.FunSuite

class BinarySearchSpec extends FunSuite {

  private val array = Array(1, 10, 20, 47, 59, 63, 75, 88, 99, 107, 120, 133, 155, 162, 176, 188, 199, 200, 210, 222)

  test("indexOf 47") {
    assert(BinarySearch.indexOf(47, array).contains(3))
  }

  test("indexOf 200") {
    assert(BinarySearch.indexOf(200, array).contains(17))
  }

  test("indexOf 201") {
    assert(BinarySearch.indexOf(201, array).isEmpty)
  }

  test("indexOfInRotatedArray") {
    val array = Array(176, 188, 199, 200, 210, 222, 1, 10, 20, 47, 59, 63, 75, 88, 99, 107, 120, 133, 155, 162)
    assert(BinarySearch.indexOfInRotatedArray(200, array).contains(3))
  }

  test("rotateArray case 1") {
    val actual = BinarySearch.rotateArray(IndexedSeq(1, 10, 20, 0, 59, 86, 32, 11, 9, 40), -1)
    val expected = IndexedSeq(10, 20, 0, 59, 86, 32, 11, 9, 40, 1)
    assert(expected == actual)
  }

  test("rotateArray case 2") {
    val actual = BinarySearch.rotateArray(IndexedSeq(1, 10, 20, 0, 59, 86, 32, 11, 9, 40), 2)
    val expected = IndexedSeq(9, 40, 1, 10, 20, 0, 59, 86, 32, 11)
    assert(expected == actual)
  }

}

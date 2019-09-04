package algorithms

import org.scalatest.FunSuite

class ArraysSpec extends FunSuite {

  val arr = Array(1, 21, 3, 14, 5, 60, 7, 6)

  test("filterPairsAddingUptoN: single match") {
    val actual = Arrays.filterPairsAddingUptoN(arr, 28)
    val expected = List((21, 7))
    assert(expected === actual)
  }

  test("filterPairsAddingUptoN: multiple matches") {
    val actual = Arrays.filterPairsAddingUptoN(arr, 8)
    val expected = List((1, 7), (3, 5))
    assert(expected === actual)
  }

  test("filterPairsAddingUptoN: no matches") {
    val actual = Arrays.filterPairsAddingUptoN(arr, 101)
    assert(actual.isEmpty)
  }

  test("productOfAllElementsExceptItself: given an array with non-zero elements") {
    val nonZeroArr = Array(-1.0, 2, -3, 4, -5)
    val actual = Arrays.productOfAllElementsExceptItself(nonZeroArr)
    val expected = Array(120, -60, 40, -30, 24)
    assert(expected === actual)
  }

  test("productOfAllElementsExceptItself: given an array with a zero element") {
    val nonZeroArr = Array(-1.0, 2, -3, 4, 0, -5)
    val actual = Arrays.productOfAllElementsExceptItself(nonZeroArr)
    val expected = Array(0, 0, 0, 0, -120, 0)
    assert(expected === actual)
  }

  test("secondMax") {
    val arr = Array(-2, -33, -10, -456)
    val actual = Arrays.secondMax(arr)
    val expected = -10
    assert(expected === actual)
  }

  test("rotateArray: negative rotations") {
    val actual = Arrays.rotateArray(IndexedSeq(1, 10, 20, 0, 59, 86, 32, 11, 9, 40), -1)
    val expected = IndexedSeq(10, 20, 0, 59, 86, 32, 11, 9, 40, 1)
    assert(expected === actual)
  }

  test("rotateArray: positive rotations") {
    val actual = Arrays.rotateArray(IndexedSeq(1, 10, 20, 0, 59, 86, 32, 11, 9, 40), 2)
    val expected = IndexedSeq(9, 40, 1, 10, 20, 0, 59, 86, 32, 11)
    assert(expected === actual)
  }

}

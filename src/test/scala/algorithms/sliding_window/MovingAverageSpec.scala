package algorithms.sliding_window

import org.scalatest.FunSuite

class MovingAverageSpec extends FunSuite {

  test("case 1") {
    val actual = MovingAverage.calculate(Stream(1, 3, 2, 6, -1, 4, 1, 8, 2), 5).toList
    val expected = List(2.2, 2.8, 2.4, 3.6, 2.8)
    assert(expected === actual)
  }

  test("case 2") {
    val actual = MovingAverage.calculate(Stream(1, 3, 2), 5).toList
    val expected = List(1.2)
    assert(expected === actual)
  }

  test("case 3") {
    val actual = MovingAverage.calculate(Stream.empty[Int], 5).toList
    assert(actual.isEmpty)
  }

}

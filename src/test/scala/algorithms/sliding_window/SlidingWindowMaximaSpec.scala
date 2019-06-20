package algorithms.sliding_window

import org.scalatest.FunSuite

class SlidingWindowMaximaSpec extends FunSuite {

  test("maxima") {
    val actual = SlidingWindowMaxima.maxima(IndexedSeq(-4, 2, -5, 3, 6), 3)
    val expected = IndexedSeq(2, 3, 6)
    assert(expected == actual)
  }

}

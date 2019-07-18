package algorithms.sliding_window

import scala.annotation.tailrec

object MovingAverage {

  // Intuition: moving average means it follows a sliding window pattern
  // Sliding window pattern involves maintaining two pointers
  // O(n) time and space
  def calculate[A](stream: Stream[A], size: Int)(implicit numeric: Numeric[A]): Iterator[Double] =
    if (stream.isEmpty) Iterator.empty
    else {
      val lastIndex = stream.length - 1

      @tailrec def loop(windowStart: Int, windowEnd: Int, windowSum: A, memo: Iterator[Double]): Iterator[Double] =
        if (windowEnd >= lastIndex) memo
        else {
          val start = stream(windowStart)
          val minusStart = numeric.minus(windowSum, start)

          val nextStart = stream(windowEnd + 1)
          val curWindowSum = numeric.plus(minusStart, nextStart)

          val avg = numeric.toDouble(curWindowSum) / size
          loop(windowStart + 1, windowEnd + 1, curWindowSum, memo ++ Iterator.single(avg))
        }

      val windowSum = stream.slice(0, size).sum
      val avg = numeric.toDouble(windowSum) / size
      loop(0, size - 1, windowSum, Iterator.single(avg))
    }

}

package algorithms.sliding_window

import scala.annotation.tailrec

object MovingAverage {

  def calculate[A](stream: Stream[A], size: Int)(implicit numeric: Numeric[A]): Iterator[Double] =
    if (stream.isEmpty) Iterator.empty
    else {
      val lastIndex = stream.length - 1

      @tailrec def loop(windowStart: Int, windowEnd: Int, windowSum: A, memo: Iterator[Double]): Iterator[Double] =
        if (windowEnd >= lastIndex) memo
        else {
          val minusStart = numeric.minus(windowSum, stream(windowStart))
          val plusEndNext = numeric.plus(minusStart, stream(windowEnd + 1))
          val avg = numeric.toDouble(plusEndNext) / size
          loop(windowStart + 1, windowEnd + 1, plusEndNext, memo ++ Iterator.single(avg))
        }

      val windowSum = stream.slice(0, size).sum
      val avg = numeric.toDouble(windowSum) / size
      loop(0, size - 1, windowSum, Iterator.single(avg))
    }

}

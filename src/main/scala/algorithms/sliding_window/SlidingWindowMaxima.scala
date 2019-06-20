package algorithms.sliding_window

import scala.annotation.tailrec

object SlidingWindowMaxima {

  def maxima[A](seq: IndexedSeq[A], windowSize: Int)(implicit ord: Ordering[A]): IndexedSeq[A] = {
    if (seq.isEmpty || windowSize < 0) IndexedSeq.empty[A]
    else if (windowSize == 0) IndexedSeq(seq.max)
    else if (windowSize == 1) seq
    else {
      @tailrec def loop(lowIdx: Int, highIdx: Int, memo: IndexedSeq[A] = IndexedSeq.empty): IndexedSeq[A] = {
        if (highIdx > seq.length) memo
        else {
          val slice = seq.slice(lowIdx, highIdx)
          if (slice.length < windowSize) memo
          else loop(lowIdx + 1, highIdx + 1, memo :+ slice.max)
        }
      }

      loop(0, windowSize)
    }
  }

}

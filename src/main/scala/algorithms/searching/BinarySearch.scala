package algorithms.searching

import scala.annotation.tailrec

object BinarySearch {

  /**
    * @param a           element to be found
    * @param sortedArray sorted array
    * @param ord         sorting order
    * @tparam A Type of array
    * @return index of the given element. None if not found.
    */
  // O(log n) time and space
  def liftIndex[A](a: A, sortedArray: Array[A])(implicit ord: Ordering[A]): Option[Int] = {
    if (sortedArray.isEmpty) None
    else {
      @tailrec def loop(lowIdx: Int, highIdx: Int): Option[Int] = {
        if (lowIdx > highIdx) None
        else {
          val midIdx = lowIdx + ((highIdx - lowIdx) / 2)
          val midVal = sortedArray(midIdx)

          if (ord.gt(midVal, a)) loop(lowIdx, midIdx - 1)
          else if (ord.lt(midVal, a)) loop(midIdx + 1, highIdx)
          else Some(midIdx)
        }
      }

      loop(0, sortedArray.length - 1)
    }
  }

  // O(log n) time and space
  def liftIndexInRotatedArray[A](a: A, rotatedArray: Array[A])(implicit ord: Ordering[A]): Option[Int] = {
    if (rotatedArray.isEmpty) None
    else {
      @tailrec def loop(lowIdx: Int, highIdx: Int): Option[Int] = {
        if (lowIdx > highIdx) None
        else {
          val midIdx = lowIdx + ((highIdx - lowIdx) / 2)
          val midVal = rotatedArray(midIdx)

          if (midVal == a) Some(midIdx)
          else {
            val lowVal = rotatedArray(lowIdx)
            val highVal = rotatedArray(highIdx)

            if (ord.lteq(lowVal, midVal) && ord.lteq(lowVal, a) && ord.lteq(a, midVal)) loop(lowIdx, midIdx - 1)
            else if (ord.lteq(midVal, highVal) && ord.lteq(midVal, a) && ord.lteq(a, highVal)) loop(midIdx + 1, highIdx)
            else if (ord.lteq(midVal, lowVal)) loop(lowIdx, midIdx - 1)
            else if (ord.gteq(midVal, highVal)) loop(midIdx + 1, highIdx)
            else None
          }
        }
      }

      loop(0, rotatedArray.length - 1)
    }
  }

}

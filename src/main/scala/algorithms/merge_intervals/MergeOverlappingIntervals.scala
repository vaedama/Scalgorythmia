package algorithms.merge_intervals

import scala.collection.mutable.ArrayBuffer

/*
Intuition:
=========
When does an overlap occur?

Given two intervals (prevFrom, prevTo) and (nextFrom, nextTo), an overlap occurs if nextFrom <= prevTo
There are two possible overlapping scenarios:

1. Full overlap: nextFrom <= prevTo and nextTo <= prevTo ..Merged = (prevFrom, prevTo)
Example: [(1,7), (4,6)] => (1,7)

2. Partial overlap: nextFrom <= prevTo and nextTo > prevTo ...Merged = (prevFrom, nextTo)
Example: [(1,5), (3,7)] => (1,7)
 */
object MergeOverlappingIntervals {

  def merge(intervals: Seq[(Int, Int)]): Seq[(Int, Int)] =
    if (intervals.isEmpty) Nil
    else {
      val sorted = intervals.sortBy(_._1)
      val result = new ArrayBuffer[(Int, Int)]()
      result += sorted.head

      sorted.tail.foreach { case (nextFrom, nextTo) =>
        val (prevFrom, prevTo) = result.last

        if (isOverlap(prevTo, nextFrom)) {
          if (!isFullOverlap(prevTo, nextTo)) {
            result.remove(result.length - 1)
            result += prevFrom -> nextTo
          }
        } else result += nextFrom -> nextTo
      }
      result
    }

  private def isOverlap(prevTo: Int, nextFrom: Int): Boolean = nextFrom <= prevTo

  private def isFullOverlap(prevTo: Int, nextTo: Int): Boolean = prevTo >= nextTo

}

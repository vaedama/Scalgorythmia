package algorithms.merge_intervals

import scala.collection.mutable.ListBuffer

// Intuition:
// When does an overlap occur?
// If prevTo >= nextFrom

// We can have two kinds of overlapping scenarios:
// 1. Full overlap (prevTo >= nextTo) => (prevFrom, prevTo)
// [(1,7), (4,6)] => (1,7)

// 2. Partial overlap => (prevFrom, nextTo)
// [(1,5), (3,7)] => (1,7)
object MergeOverlappingIntervals {

  def merge(sortedIntervals: Seq[(Int, Int)]): Seq[(Int, Int)] = sortedIntervals match {
    case Nil => Nil
    case hd :: tail =>
      val result = new ListBuffer[(Int, Int)]
      result += hd

      tail.foreach { case (nextFrom, nextTo) =>
        val (prevFrom, prevTo) = result.last

        if (isOverlap(prevTo, nextFrom)) {
          if (!isFullOverlap(prevTo, nextTo)) {
            result.remove(result.length - 1)
            result += prevFrom -> nextTo
          }
        } else result += nextFrom -> nextTo

      }

      result.toList
  }

  private def isOverlap(prevTo: Int, nextFrom: Int): Boolean = prevTo >= nextFrom

  private def isFullOverlap(prevTo: Int, nextTo: Int): Boolean = prevTo >= nextTo

}

object MergeOverlappingIntervalsDemo extends App {



}


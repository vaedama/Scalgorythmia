package algorithms.sorting

import scala.annotation.tailrec
import scala.collection.mutable

object MergeSortedLists {

  def merge(seq1: Seq[Int], seq2: Seq[Int]): Seq[Int] = {
    @tailrec def loop(rem1: Seq[Int], rem2: Seq[Int], memo: Vector[Int] = Vector.empty): Seq[Int] = (rem1, rem2) match {
      case (Seq(), Seq()) => memo
      case (Seq(), _) => memo ++ rem2
      case (_, Seq()) => memo ++ rem1
      case (Seq(h1, t1@_*), Seq(h2, t2@_*)) =>
        if (h1 == h2) loop(t1, t2, memo :+ h1 :+ h2)
        else if (h1 < h2) loop(t1, rem2, memo :+ h1)
        else loop(rem1, t2, memo :+ h2)
    }

    loop(seq1, seq2)
  }

  def mergeN(lists: List[List[Int]]): List[Int] = {
    val listIters: List[Iterator[Int]] = lists.map(_.toIterator)
    // so we can consume the lists nicely

    case class ListEntry(value: Int, listIdx: Int)
    // so we can keep track of which pointer to increment

    val minHeap = mutable.PriorityQueue.empty[ListEntry](Ordering.by(-_.value))
    // so we can keep track of min item

    listIters.zipWithIndex.foreach { case (iter, idx) =>
      if (iter.hasNext) minHeap.enqueue(ListEntry(iter.next, idx))
    }
    // so we can initialize our min heap with the head of each iterator

    val result = mutable.ListBuffer.empty[Int]
    // so we can keep track of our results

    while (minHeap.nonEmpty) {
      val ListEntry(min, idx) = minHeap.dequeue
      result += min

      val minIter = listIters(idx)
      if (minIter.hasNext) minHeap.enqueue(ListEntry(minIter.next, idx))
    }

    result.toList
  }

}

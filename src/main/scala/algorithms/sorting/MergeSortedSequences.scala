package algorithms.sorting

import scala.annotation.tailrec
import scala.collection.mutable

object MergeSortedSequences {

  def merge[A](seq1: Seq[A], seq2: Seq[A])(implicit ord: Ordering[A]): Seq[A] = {
    import ord._
    @tailrec def loop(rem1: Seq[A], rem2: Seq[A], memo: Vector[A] = Vector.empty): Seq[A] = (rem1, rem2) match {
      case (Nil, Nil) => memo
      case (Nil, _) => memo ++ rem2
      case (_, Nil) => memo ++ rem1
      case (Seq(h1, t1@_*), Seq(h2, t2@_*)) =>
        if (h1 == h2) loop(t1, t2, memo :+ h1 :+ h2)
        else if (h1 < h2) loop(t1, rem2, memo :+ h1)
        else loop(rem1, t2, memo :+ h2)
    }

    loop(seq1, seq2)
  }

  // so we can keep track of which pointer to increment
  private case class SeqEntry(value: Int, listIdx: Int)

  def mergeN(seqs: Seq[Seq[Int]]): Seq[Int] = {
    // convert each seq to an iterator we can consume the elements nicely
    val iteratorSeq: Seq[Iterator[Int]] = seqs.map(_.toIterator)

    // create a min-heap to eject/dequeue the min item
    val minHeap = mutable.PriorityQueue.empty[SeqEntry](Ordering.by(-_.value))

    // initialize our min heap with the head of each iterator
    iteratorSeq.zipWithIndex.foreach { case (iterator, idx) =>
      if (iterator.hasNext) minHeap.enqueue(SeqEntry(iterator.next, idx))
    }

    val result = mutable.ListBuffer.empty[Int]

    while (minHeap.nonEmpty) {
      val SeqEntry(min, idx) = minHeap.dequeue
      result += min

      val minIterator = iteratorSeq(idx)
      if (minIterator.hasNext) minHeap.enqueue(SeqEntry(minIterator.next, idx))
    }

    result
  }

}

package datastructures.immutable.queue

import algorithms.sorting.MergeSort
import datastructures.immutable.stack.Stack

/*
Functional implementation of Queue aka Banker's Queue (LIFO structure)
Queue Applications:
- BFS
- Priority Queue: Priority switching between processes by OS
- Circular queue: Event handling
 */
case class Queue[A] private(
  in: List[A],
  out: List[A],
  _size: Int) {

  // O(1) time and space
  def enqueue[B >: A](b: B): Queue[B] = copy(b :: in, out, _size + 1)

  // O(1) time and space amortized
  def dequeue: (Option[A], Queue[A]) = out match {
    case h :: t => Some(h) -> copy(in, t, _size - 1)
    case Nil => in.reverse match {
      case h :: t => Some(h) -> copy(Nil, t, _size - 1)
      case Nil => None -> copy(Nil, Nil, 0)
    }
  }

  // O(1) time and space
  def front: Option[A] = dequeue._1

  // O(1) time and space
  def rear: Queue[A] = dequeue._2

  // O(1) time and space
  def isEmpty: Boolean = _size == 0

  // O(1) time and space
  def size: Int = _size

  // O(n) time and space
  def reverse(n: Int): Queue[A] = {
    val (left, right) = toList.splitAt(n)
    new Queue(Nil, left.reverse ++ right, _size)
  }

  def mkString: String = {
    if (in.isEmpty && out.isEmpty) "Queue()"
    else s"Queue(${toList.mkString})"
  }

  // O(1) time and space amortized
  def toList: List[A] = out ++ in.reverse

  // O(n log n) time and space
  def sorted(implicit ord: Ordering[A]): Queue[A] = copy(Nil, MergeSort.sort(toList).toList)

}

object Queue {

  // O(1) time and space
  def empty[A]: Queue[A] =
    new Queue(Nil, Nil, 0)

  // O(n) time and O(1) space
  def apply[A](as: A*): Queue[A] =
    as.foldLeft(empty[A])(_ enqueue _)

  // O(n) time and space
  def apply[A](stack: Stack[A]): Queue[A] =
    apply(stack.toList.reverse: _*)

}

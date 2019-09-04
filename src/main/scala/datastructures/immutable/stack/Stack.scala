package datastructures.immutable.stack

import algorithms.sorting.MergeSort

// LIFO data structure
// Purely functional implementation of Stack

// Applications:
// 1. DFS
// 2. Expression evaluation algorithms
case class Stack[A] private(
  as: List[A],
  _size: Int) {

  // O(1) time and space
  def pop: Option[(A, Stack[A])] = as match {
    case Nil => None
    case h :: t => Some(h, new Stack(t, _size - 1))
  }

  // O(1) time and space
  def push(a: A): Stack[A] = new Stack(a :: as, _size + 1)

  // O(1) time and space
  def top: Option[A] = as.headOption

  // O(1) time and space
  def bottom: Stack[A] = pop.map(_._2).getOrElse(Stack.empty[A])

  // O(1) time and space
  def isEmpty: Boolean = _size == 0

  // O(1) time and space
  def toList: List[A] = as

  // O(1) time and space
  def size: Int = _size

}

object Stack {

  // O(1) time and space
  def empty[A]: Stack[A] = new Stack(Nil, 0)

  // O(n) time and O(1) space
  def apply[A](as: A*): Stack[A] = as.foldLeft(empty[A])(_ push _)

  // O(n log n) time and space
  def sort[A](stack: Stack[A])(implicit ordering: Ordering[A]): Stack[A] =
    apply(MergeSort.sort[A](stack.toList): _*)

}

package datastructures.immutable.stack

import algorithms.sorting.MergeSort
import datastructures.immutable.queue.Queue

/*
Purely functional implementation of the LIFO data structure: Stack
Applications:
1. DFS
2. Expression evaluation algorithms
*/
case class Stack[A] private(
  as: List[A],
  _size: Int) {

  // O(1) time and space
  def pop: Option[(A, Stack[A])] = as match {
    case Nil => None
    case h :: t => Some(h, copy(t, _size - 1))
  }

  // O(1) time and space
  def push(a: A): Stack[A] = copy(a :: as, _size + 1)

  // O(1) time and space
  def top: Option[A] = as.headOption

  // O(1) time and space
  def bottom: Stack[A] = pop.fold(Stack.empty[A])(_._2)

  // O(1) time and space
  def isEmpty: Boolean = _size == 0

  // O(1) time and space
  def size: Int = _size

  // O(1) time and space
  def toList: List[A] = as

  // O(n log n) time and space
  def sorted(implicit ord: Ordering[A]): Stack[A] = copy(MergeSort.sort(as).toList, _size)

}

object Stack {

  // O(1) time and space
  def empty[A]: Stack[A] = new Stack(Nil, 0)

  // O(n) time and O(1) space
  def apply[A](as: A*): Stack[A] = as.foldLeft(empty[A])(_ push _)

}

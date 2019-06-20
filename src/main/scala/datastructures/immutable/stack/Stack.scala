package datastructures.immutable.stack

import algorithms.sorting.MergeSort

// LIFO data structure
// Functional implementation of Stack

// Applications:
// - DFS
// - Expression Evaluation Algorithms
case class Stack[+A](self: List[A]) {

  // O(1) time and space
  def pop: (Option[A], Stack[A]) = (top, bottom)

  // O(1) time and space
  def push[B >: A](b: B): Stack[B] = new Stack(b :: self)

  // O(1) time and space
  def top: Option[A] = self.headOption

  // O(1) time and space
  def bottom: Stack[A] = new Stack(self.tail)

  // O(1) time and space
  def isEmpty: Boolean = self.isEmpty

  // O(1) time and space
  def toList: List[A] = self

  // O(1) time and space
  def size: Int = self.size

}

object Stack {

  // O(1) time and space
  def empty[A]: Stack[A] = new Stack(Nil)

  // O(n) time and O(1) space
  def apply[A](as: A*): Stack[A] = as.foldLeft(empty[A])(_ push _)

  // O(n log n) time and space
  def sort[A](stack: Stack[A])(implicit ordering: Ordering[A]): Stack[A] =
    Stack(MergeSort.sort[A](stack.toList).toList)

}

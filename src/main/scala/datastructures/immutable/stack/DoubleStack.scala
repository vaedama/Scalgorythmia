package datastructures.immutable.stack

import algorithms.sorting.MergeSort

// LIFO structure
// Implements 2 stacks using one array
// Here we use a Vector instead of an array for better performance of "push1" operation
// In Scala, a Vector is implemented as a 32-ary tree
case class DoubleStack[A] private(
  self: Vector[A],
  _size: Int) {

  // O(log32 (n)) time and O(1) space
  def push1(a: A): DoubleStack[A] = DoubleStack(a +: self, _size + 1)

  // O(log32 (n)) time and O(1) space
  def push2(a: A): DoubleStack[A] = DoubleStack(self :+ a, _size + 1)

  // O(log32 (n)) time and O(1) space
  def pop1: (Option[A], DoubleStack[A]) = {
    if (self.isEmpty) (None, this)
    else (self.headOption, DoubleStack(self.tail, _size - 1))
  }

  // O(log32 (n)) time and O(1) space
  def pop2: (Option[A], DoubleStack[A]) = {
    if (self.isEmpty) (None, this)
    else (self.lastOption, DoubleStack(self.dropRight(1), _size - 1))
  }

  // O(log32 (n)) time and space
  def contains(a: A): Boolean = self.contains(a)

  // O(log32 (n)) time and space
  def exists(f: A => Boolean): Boolean = self.exists(f)

  // O(1) time and space
  def isEmpty: Boolean = _size == 0

  // O(1) time and space
  def toList: List[A] = self.toList

  // O(1) time and space
  def size: Int = _size

}

object DoubleStack {

  // O(1) time and space
  def empty[A]: DoubleStack[A] = new DoubleStack(Vector.empty, 0)

  // O(n) time and O(1) space
  def apply[A](as: A*): DoubleStack[A] = as.zipWithIndex.foldLeft(empty[A]) { case (memo, (a, i)) =>
    if (i % 2 == 0) memo.push1(a) else memo.push2(a)
  }

  // O(n log n) time and space
  def sort[A](stack: DoubleStack[A])(implicit ordering: Ordering[A]): DoubleStack[A] = {
    DoubleStack(MergeSort.sort[A](stack.toList).toVector, stack.size)
  }

}

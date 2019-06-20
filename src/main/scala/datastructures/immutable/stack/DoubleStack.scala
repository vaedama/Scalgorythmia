package datastructures.immutable.stack

import algorithms.sorting.MergeSort

// LIFO structure
// Implements 2 stacks using one array
case class DoubleStack[A](private val self: Vector[A]) {

  def push1(a: A): DoubleStack[A] = DoubleStack(a +: self)

  def push2(a: A): DoubleStack[A] = DoubleStack(self :+ a)

  def pop1: (Option[A], DoubleStack[A]) = {
    if (self.isEmpty) (None, this)
    else (self.headOption, DoubleStack(self.tail))
  }

  def pop2: (Option[A], DoubleStack[A]) = {
    if (self.isEmpty) (None, this)
    else (self.lastOption, DoubleStack(self.dropRight(1)))
  }

  def isEmpty: Boolean = self.isEmpty

  def toList: List[A] = self.toList

  def size: Int = self.size

}

object DoubleStack {

  // O(1) time and space
  def empty[A]: DoubleStack[A] = new DoubleStack(Vector.empty)

  // O(n) time and O(1) space
  def apply[A](as: A*): DoubleStack[A] = as.zipWithIndex.foldLeft(empty[A]) { case (memo, (a, i)) =>
    if (i % 2 == 0) memo.push1(a) else memo.push2(a)
  }

  // O(n log n) time and space
  def sort[A](stack: DoubleStack[A])(implicit ordering: Ordering[A]): DoubleStack[A] =
    DoubleStack(MergeSort.sort[A](stack.toList).toVector)

}

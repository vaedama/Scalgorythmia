package datastructures.immutable.stack

import algorithms.sorting.MergeSort

/*
LIFO stack that implements two stacks using one array.
Here we use Vector instead of array for better performance of "push1" operation
Note that in Scala, a Vector is implemented as a 32-ary tree
 */
case class DoubleStack[A] private(
  as: Vector[A],
  _size: Int) {

  // O(log32 n) time and O(1) space
  def push1(a: A): DoubleStack[A] = copy(a +: as, _size + 1)

  // O(log32 n) time and O(1) space
  def push2(a: A): DoubleStack[A] = copy(as :+ a, _size + 1)

  // O(log32 n) time and O(1) space
  def pop1: (Option[A], DoubleStack[A]) =
    if (isEmpty) (None, this)
    else (as.headOption, copy(as.tail, _size - 1))

  // O(log32 n) time and O(1) space
  def pop2: (Option[A], DoubleStack[A]) =
    if (isEmpty) (None, this)
    else (as.lastOption, copy(as.dropRight(1), _size - 1))

  // O(log32 n) time and space
  def contains(a: A): Boolean = as.contains(a)

  // O(log32 n) time and space
  def exists(f: A => Boolean): Boolean = as.exists(f)

  // O(1) time and space
  def isEmpty: Boolean = _size == 0

  // O(1) time and space
  def size: Int = _size

  // O(1) time and space
  def toList: List[A] = as.toList

  // O(n log n) time and space
  def sorted(implicit ord: Ordering[A]): DoubleStack[A] = copy(MergeSort.sort(as).toVector)

}

object DoubleStack {

  // O(1) time and space
  def empty[A]: DoubleStack[A] =
    new DoubleStack(Vector.empty, 0)

  // O(n) time and O(1) space
  def apply[A](as: A*): DoubleStack[A] =
    as.zipWithIndex.foldLeft(empty[A]) { case (memo, (a, i)) =>
      if (i % 2 == 0) memo.push1(a) else memo.push2(a)
    }

}

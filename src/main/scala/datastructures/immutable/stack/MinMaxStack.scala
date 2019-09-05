package datastructures.immutable.stack

/*
LIFO stack that implements min and max operations in constant time
by burying the min and max values as a tuple in the constructor.
 */
case class MinMaxStack[A] private(
  as: List[A],
  minMaxList: List[(A, A)],
  _size: Int)(implicit ord: Ordering[A]) {

  import ord._

  private def minMax: Option[(A, A)] = minMaxList.headOption

  // O(1) time and space
  def min: Option[A] = minMax.map(_._1)

  // O(1) time and space
  def max: Option[A] = minMax.map(_._2)

  // O(1) time and space
  def push(a: A): MinMaxStack[A] = minMax match {
    case None =>
      copy(a :: as, (a, a) :: minMaxList, _size + 1)
    case Some((min, max)) =>
      val newMin = if (a < min) a else min
      val newMax = if (a > max) a else max
      copy(a :: as, (newMin, newMax) :: minMaxList, _size + 1)
  }

  // O(1) time and space
  def pop: Option[(A, MinMaxStack[A])] = as match {
    case Nil => None
    case h :: t => Some(h, copy(t, minMaxList.tail, _size - 1))
  }

  // O(1) time and space
  def top: Option[A] = as.headOption

  // O(1) time and space
  def bottom: MinMaxStack[A] = pop.fold(MinMaxStack.empty[A])(_._2)

  // O(1) time and space
  def isEmpty: Boolean = _size == 0

  // O(1) time and space
  def size: Int = _size

}

object MinMaxStack {

  def empty[A](implicit ord: Ordering[A]): MinMaxStack[A] =
    new MinMaxStack[A](Nil, Nil, 0)

  def apply[A](as: A*)(implicit ord: Ordering[A]): MinMaxStack[A] =
    as.foldLeft(empty[A])(_ push _)

}

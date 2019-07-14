package datastructures.immutable.stack

// LIFO structure
// Implements min and max operations in constant time
case class MinMaxStack[A] private(as: List[A], minMaxList: List[(A, A)], _size: Int)(implicit ord: Ordering[A]) {

  // O(1) time and space
  def min: Option[A] = minMaxList.headOption.map(_._1)

  // O(1) time and space
  def max: Option[A] = minMaxList.headOption.map(_._2)

  // O(1) time and space
  def push(a: A): MinMaxStack[A] = minMaxList match {
    case Nil =>
      new MinMaxStack(a :: as, (a, a) :: minMaxList, _size + 1)
    case (curMin, curMax) :: _ =>
      val newMin = if (ord.lt(a, curMin)) a else curMin
      val newMax = if (ord.gt(a, curMax)) a else curMax
      new MinMaxStack(a :: as, (newMin, newMax) :: minMaxList, _size + 1)
  }

  // O(1) time and space
  def pop: Option[(A, MinMaxStack[A])] = as match {
    case Nil => None
    case h :: t => Some(h, new MinMaxStack(t, minMaxList.tail, _size - 1))
  }

  // O(1) time and space
  def top: Option[A] = pop.map(_._1)

  // O(1) time and space
  def bottom: MinMaxStack[A] = pop.map(_._2).getOrElse(MinMaxStack.empty[A])

  // O(1) time and space
  def isEmpty: Boolean = _size == 0

  // O(1) time and space
  def size: Int = _size

}

object MinMaxStack {

  def empty[A](implicit ord: Ordering[A]): MinMaxStack[A] = new MinMaxStack[A](Nil, Nil, 0)

  def apply[A](as: A*)(implicit ord: Ordering[A]): MinMaxStack[A] = as.foldLeft(empty[A])(_ push _)

}

package datastructures.immutable.list

import scala.annotation.tailrec

// Purely functional implementation of singly-linked-list

// The covariant type +A allows us to say:
// val strings: List[String] = Nil
sealed trait List[+A] {

  // O(1)
  def head: A

  // O(1)
  def tail: List[A]

  // O(1)
  def isEmpty: Boolean

  // O(1)
  def prepend[B >: A](b: B): List[B] = ::(b, this)

  // O(n) time and space
  def append[B >: A](b: B): List[B] = this match {
    case Nil => ::(b)
    case h :: t => ::(h, t.append(b))
  }

  // O(n) time complexity with O(1) space complexity
  def reverse: List[A] = {
    @tailrec def loop(in: List[A], memo: List[A] = Nil): List[A] = in match {
      case Nil => memo
      case h :: t => loop(t, memo.prepend(h))
    }

    loop(this)
  }

  // O(n) time and space
  def lift(index: Int): Option[A] = this match {
    case Nil => None
    case h :: _ if index == 0 => Some(h)
    case _ :: t => t.lift(index - 1)
  }

  // O(n) time complexity with O(1) space complexity
  // because the tail call consumes the same stack frame
  def drop(n: Int): List[A] = {
    if (n <= 0) this
    else {
      @tailrec def loop(in: List[A], rem: Int): List[A] =
        if (rem == 0) in
        else in match {
          case Nil => in
          case _ :: t => loop(t, rem - 1)
        }

      loop(this, n)
    }
  }

  // O(n) time and space
  def dropWhile(f: A => Boolean): List[A] = this match {
    case h :: t if f(h) => t.dropWhile(f)
    case _ => this
  }

  // O(n) time and space
  def take(n: Int): List[A] =
    if (n <= 0) Nil
    else this match {
      case h :: t => ::(h, t.take(n - 1))
      case _ => this
    }

  // O(n) time and space
  def takeWhile(f: A => Boolean): List[A] = this match {
    case h :: t if f(h) => t.takeWhile(f)
    case _ => this
  }

  // O(n) time and space
  def size: Int = this match {
    case Nil => 0
    case _ :: t => 1 + t.size
  }

  // O(n) time and space
  def updated[B >: A](index: Int, b: => B): List[B] = {
    if (index >= size) throw new IndexOutOfBoundsException(s"List size=$size")
    this match {
      case Nil => Nil
      case _ :: t if index == 0 => t.prepend(b)
      case h :: t => ::(h, t.updated(index - 1, b))
    }
  }

  // O(n) time and space
  def remove[B >: A](b: B): List[B] = this match {
    case Nil => Nil
    case h :: t => if (h == b) t else t.remove(b).prepend(h)
  }

  // O(n) time and space
  def sum[B >: A](implicit numeric: Numeric[B]): B = this match {
    case Nil => numeric.zero
    case h :: t => numeric.plus(h, t.sum(numeric))
  }

  // O(n) time and space
  def product[B >: A](implicit numeric: Numeric[B]): B = this match {
    case Nil => numeric.one
    case h :: _ if h == numeric.zero => numeric.zero
    case h :: t => numeric.times(h, t.product(numeric))
  }

}

case object Nil extends List[Nothing] {
  override def head: Nothing = throw new NoSuchElementException("head on Nil")

  override def tail: List[Nothing] = throw new NoSuchElementException("tail on Nil")

  override def isEmpty: Boolean = true
}

case class ::[+A](head: A, tail: List[A] = Nil) extends List[A] {
  override def isEmpty: Boolean = false
}

object List {

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else ::(as.head, apply[A](as.tail: _*))

}



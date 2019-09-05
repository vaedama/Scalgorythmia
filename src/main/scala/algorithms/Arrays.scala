package algorithms

import scala.annotation.tailrec

object Arrays {

  /*
  Intuition:
  ==========
  Use a Set to lookup a complement occurrence.
  If the complement exists, add the element and it's complement to the pairs list.
  Otherwise, add the occurrence to the occurrence set and iterate until the end of array.

  O(n) time and space where "n" is size of the array
   */
  def filterPairsAddingUptoN[A](arr: Vector[A], n: A)(implicit num: Numeric[A]): List[(A, A)] = {
    import num._

    @tailrec def loop(idx: Int, occurrences: Set[A] = Set.empty[A], memo: List[(A, A)] = Nil): List[(A, A)] =
      if (arr.length == idx) memo
      else {
        val element = arr(idx)
        val complement = n - element
        if (occurrences(complement)) loop(idx + 1, occurrences, (complement, element) :: memo)
        else loop(idx + 1, occurrences + element, memo)
      }

    loop(0)
  }

  /*
  Intuition:
  ==========
  Product of all elements except itself is same as product of all elements divided by itself.
  Handle div-by-zero exception by lazily computing the non-zero product of the array and
  using it instead of the product for "zero" encounters.

  O(n) time and space where "n" is size of the array
   */
  def productOfAllElementsExceptItself[A](arr: Vector[A])(implicit fract: Fractional[A]): Vector[A] = {
    val product: A = arr.product

    import fract._
    lazy val nonZeroProduct: A = arr.filter(_ != zero).product
    arr.map(a => if (a != zero) product / a else nonZeroProduct)
  }

  /*
  Intuition:
  ==========
  To compute the secondMax, keep track of max and secondMax. While traversing the array,
  if the current element is larger than max then max becomes current and secondMax becomes max.
  If the current element is smaller than max and larger than secondMax, max remains the same and
  the current element becomes secondMax.

  O(n) time and space where "n" is size of the array
   */
  def secondMax[A](arr: Vector[A])(implicit num: Numeric[A]): A =
    arr match {
      case Vector() => throw new UnsupportedOperationException("secondMax not supported on empty array")
      case Vector(single) => single
      case Vector(f, s, r@_*) =>
        import num._

        @tailrec def loop(max: A, secondMax: A, rem: Seq[A]): A = rem match {
          case Seq() => secondMax
          case Seq(h, t@_*) =>
            if (h >= max) loop(h, max, t)
            else if (max >= h && h >= secondMax) loop(max, h, t)
            else loop(max, secondMax, t)
        }

        if (f >= s) loop(f, s, r)
        else loop(s, f, r)
    }

  /*
  Intuition:
  ==========
  Rotations can be positive or negative
  If rotations is positive, rotate the array clockwise:
  Last element becomes head. This last element is prepended to the remaining array.

  If rotations is negative, rotate the array counter-clockwise:
  First element becomes last. This first element is appended to the tail array.

  O(n) time and space where "n" is number of rotations
   */
  def rotateArray[A](arr: Vector[A], rotations: Int): Vector[A] =
    arr match {
      case Vector() => arr
      case Vector(_) => arr
      case _ =>
        @tailrec def loop(rem: Int, memo: Vector[A]): Vector[A] = {
          if (rem == 0) memo
          else if (rem > 0) loop(rem - 1, memo.last +: memo.dropRight(1))
          else loop(rem + 1, memo.tail :+ memo.head)
        }

        loop(rotations, arr)
    }

}

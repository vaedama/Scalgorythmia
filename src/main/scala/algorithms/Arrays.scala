package algorithms

import scala.annotation.tailrec
import scala.reflect.ClassTag

object Arrays {

  // O(n) time and space complexity
  // Intuition: Use hash set to look for complement occurrence.
  // If complement exists, add the pair to the pairs list.
  // Otherwise, add the occurrence to the occurrence set.
  def filterPairsAddingUptoN[A](arr: Array[A], n: A)(implicit num: Numeric[A]): List[(A, A)] = {
    @tailrec def loop(curIdx: Int, occurrences: Set[A] = Set.empty[A], memo: List[(A, A)] = Nil): List[(A, A)] =
      if (arr.length == curIdx) memo
      else {
        val element = arr(curIdx)
        val complement = num.minus(n, element)
        if (occurrences(complement)) loop(curIdx + 1, occurrences, (complement, element) :: memo)
        else loop(curIdx + 1, occurrences + element, memo)
      }

    loop(0)
  }

  // O(n) time and space complexity
  // Intuition: If the array contains zero value, dividing by 0 will throw ArithmeticException
  // Avoid that by lazily computing non-zero product of the array and using it if we
  // encounter zero while traversing the array.
  def productOfAllElementsExceptItself[A: ClassTag](arr: Array[A])(implicit fract: Fractional[A]): Array[A] = {
    val product: A = arr.product
    lazy val nonZeroProduct: A = arr.filter(_ != 0).product
    arr.map(a => if (a != 0) fract.div(product, a) else nonZeroProduct)
  }

  // O(n) time and space complexity
  // Intuition: Keep track of max and secondMax
  // If the next element is larger than max, max becomes this element and secondMax becomes max
  // Otherwise, if the next element is smaller than max but larger than secondMax, this element
  // becomes secondMax. Continue the iteration until the end of the array is reached.
  def secondMax[A: ClassTag](array: Array[A])(implicit num: Numeric[A]): A =
    array match {
      case Array() => throw new UnsupportedOperationException("secondMax not supported on empty array")
      case Array(single) => single
      case Array(f, s, r@_*) =>
        @tailrec def loop(max: A, secondMax: A, rem: Array[A]): A = rem match {
          case Array() => secondMax
          case Array(head, _*) =>
            if (num.gteq(head, max)) loop(head, max, rem.tail)
            else if (num.gteq(max, head) && num.gteq(head, secondMax)) loop(max, head, rem.tail)
            else loop(max, secondMax, rem.tail)
        }

        if (num.gteq(f, s)) loop(f, s, r.toArray)
        else loop(s, f, r.toArray)
    }

}

package algorithms.conversions

import scala.annotation.tailrec

object MathConversions {

  /*
   q = quotient
   r = remainder

   Example 1: a = 3
   3 : 2 => q = 1, r = 1
   1 : 2 => q = 0, r = 1
   binary = 11

   Example 2: a = 12
   12 : 2 => q = 6; r = 0
   6 : 2 => q = 3; r = 0
   3 : 2 => q = 1; r = 1
   1 : 2 => q = 0; r = 1
   binary = 1100
   */
  def decimalToBinary[A](a: A)(implicit integral: Integral[A]): BigInt = {
    val two = integral.plus(integral.one, integral.one)
    def quot(i: A): A = integral.quot(i, two)
    def rem(i: A): A = integral.rem(i, two)

    @tailrec def loop(quotient: A, memo: String): BigInt =
      if (quotient == 0) BigInt(memo)
      else loop(quot(quotient), rem(quotient) + memo)

    loop(quot(a), rem(a).toString)
  }

  /*
  binary = 1011
  2 * 0 + 1 = 1
  2 * 1 + 0 = 2
  2 * 2 + 1 = 5
  2 * 5 + 1 = 11
  decimal = 11
  */
  def binaryToDecimal(binary: BigInt): Int = {
    @tailrec def loop(remBin: String, sum: Int): Int =
      if (remBin.isEmpty) sum
      else loop(remBin.tail, (2 * sum) + Integer.parseInt(s"${remBin.head}", 10))

    loop(binary.toString, 0)
  }

  // modular arithmetic
  def getNthDigit(number: Int, base: Int, n: Int): Int =
    ((number / Math.pow(base, n)) % 10).toInt

}

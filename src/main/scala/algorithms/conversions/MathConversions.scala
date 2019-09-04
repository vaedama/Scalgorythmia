package algorithms.conversions

import scala.annotation.tailrec

object MathConversions {

  /*
  decimalToBinary intuition:
  ==========================
  1. Divide decimal by 2 => get quotient
  2. Modular divide decimal by 2 => get reminder
  3. Repeat steps (1) and (2) until quotient is zero by accumulating the reminders at each level
  4. Convert the reminders to a BigInt

  q = quotient
  r = remainder

  Example 1:
  ----------
   i = 3
   3 : 2 => q = 1, r = 1
   1 : 2 => q = 0, r = 1
   binary = 11

   Example 2:
  ----------
   i = 12
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

    @tailrec def loop(q: A, memo: String): BigInt =
      if (q == 0) BigInt(memo)
      else loop(quot(q), rem(q) + memo)

    loop(quot(a), rem(a).toString)
  }

  /*
  binaryToDecimal intuition:
  ==========================
  1. Consume the binary input element by element by adding it to accumulator multiplied by 2
  2. Repeat until the end of binary input is reached
  3. Return the accumulated sum.

  Example 1:
  ----------
  binary = 1011
  2 * 0 + 1 = 1
  2 * 1 + 0 = 2
  2 * 2 + 1 = 5
  2 * 5 + 1 = 11
  decimal = 11
  */
  def binaryToDecimal(binary: BigInt): Int = {
    @tailrec def loop(bin: String, sum: Int): Int =
      if (bin.isEmpty) sum
      else loop(bin.tail, (2 * sum) + bin.head.toString.toInt)

    loop(binary.toString, 0)
  }

  // modular arithmetic
  def nthDigit(number: Int, base: Int, n: Int): Int =
    ((number / Math.pow(base, n)) % 10).toInt

}

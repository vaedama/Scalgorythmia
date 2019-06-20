package algorithms.conversions

import scala.annotation.tailrec

object MathConversions {

  /*
   i = 3
   3 : 2 => q = 1, r = 1
   1 : 2 => 1 = 0, r = 1
   binary = 11
   ---------------------
   i = 12
   12 : 2 => q = 6; r = 0
   6 : 2 => q = 3; r = 0
   3 : 2 => q = 1; r = 1
   1 : 2 => q = 0; r = 1
   binary = 1100
   */
  def decimalToBinary(decimal: Int): BigInt = {
    @tailrec def loop(quotient: Int, memo: String): BigInt = {
      if (quotient == 0) BigInt(memo)
      else loop(quotient / 2, (quotient % 2) + memo)
    }

    loop(decimal / 2, (decimal % 2).toString)
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
    @tailrec def loop(remBin: String, sum: Int): Int = {
      if (remBin.isEmpty) sum
      else loop(remBin.tail, (2 * sum) + Integer.parseInt(s"${remBin.head}", 10))
    }

    loop(binary.toString, 0)
  }

  // modular arithmetic
  def getNthDigit(number: Int, base: Int, n: Int): Int = {
    ((number / Math.pow(base, n)) % 10).toInt
  }

}

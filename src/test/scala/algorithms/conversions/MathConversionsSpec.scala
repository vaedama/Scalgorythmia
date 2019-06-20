package algorithms.conversions

import org.scalatest.FunSuite

class MathConversionsSpec extends FunSuite {

  test("binaryToDecimal") {
    MathConversions.binaryToDecimal(BigInt(1011)) == 12
  }

  test("decimalToBinary") {
    MathConversions.decimalToBinary(12) == BigInt(1011)
  }

}

package algorithms.conversions

import org.scalatest.FunSuite

class MathConversionsSpec extends FunSuite {

  test("binaryToDecimal") {
    assert(MathConversions.binaryToDecimal(BigInt(1011)) === 11)
  }

  test("decimalToBinary") {
    assert(MathConversions.decimalToBinary(11) === BigInt(1011))
  }

  test("getNthDigit") {
    assert(MathConversions.nthDigit(1024, 10, 0) === BigInt(4))
    assert(MathConversions.nthDigit(1024, 10, 1) === BigInt(2))
    assert(MathConversions.nthDigit(1024, 10, 2) === BigInt(0))
    assert(MathConversions.nthDigit(1024, 10, 3) === BigInt(1))
  }

}

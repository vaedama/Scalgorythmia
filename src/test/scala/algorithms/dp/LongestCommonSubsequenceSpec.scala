package algorithms.dp

import org.scalatest.FunSuite

class LongestCommonSubsequenceSpec extends FunSuite {

  test("lcs for two strings") {
    val s1 = "XKYKZPW"
    val s2 = "ZXVVYZW"
    val expected = "XYZW"
    assert((new LongestCommonSubsequence).lcs(s1.toList, s2.toList).mkString == expected)
  }

}

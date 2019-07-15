package algorithms.dp

import scala.language.implicitConversions

// Intuition:
// Start by looking at the final characters of each of the string

// If both strings end in the same character, the answer must end in last character
// Build the LCS with the last character and prepend the future matches

// If they are not equal to each other, we find the LCS by removing one or the other
// character and check if they are equal

// Build a 2D matrix with the given strings
// Our rows and columns start with empty strings - they serve as base case
// Fill-up /
// Is Z == X => No
// ...
/*
 "" X K Y K Z P W
""/ / / / / / / /
Z / / / / / Z Z Z
X / X X X X X X X
V / X X X X X X X
V / X X X X X X X
Y / X X xy = = = =
Z / X X xy xyz = =
W / X X xy xyz = xyzw
 */
class LongestCommonSubsequence[A] {

  private type DP = Memo[(List[A], List[A]), (Int, Int), List[A]]

  private def key(input: (List[A], List[A])): (Int, Int) = (input._1.length, input._2.length)

  private val ord: Ordering[List[A]] = Ordering.by(_.length)

  private lazy val f: DP = Memo(key) {
    case (Nil, _) | (_, Nil) => Nil
    case (x :: xs, y :: ys) if x == y => x :: f(xs, ys)
    case (xs, ys) => ord.max(f(xs.tail, ys), f(xs, ys.tail))
  }

  // O(l1.length * l2.length)
  def lcs(l1: List[A], l2: List[A]): List[A] = f(l1, l2)

}

case class Memo[I, K, O](ik: I => K)(io: I => O) extends (I => O) {

  val cache = scala.collection.mutable.Map.empty[K, O]

  override def apply(i: I): O = cache.getOrElseUpdate(ik(i), io(i))

}

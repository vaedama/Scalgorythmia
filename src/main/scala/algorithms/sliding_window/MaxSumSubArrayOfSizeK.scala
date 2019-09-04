package algorithms.sliding_window

object MaxSumSubArrayOfSizeK {

  // Intuition: maintain two pointers for sub-array problems
  // O(n) time and space
  def maxSum[N](array: Array[N], k: Int)(implicit numeric: Numeric[N]): N = {
    if (k <= 0) throw new IllegalArgumentException("k must be > 0")

    var start = 0
    var end = k - 1

    val sum = array.slice(start, k).sum
    var curSum = sum
    var maxSum = sum

    while (end < array.length - 1) {
      end += 1
      curSum = numeric.plus(numeric.minus(curSum, array(start)), array(end))
      maxSum = if (numeric.gt(curSum, maxSum)) curSum else maxSum
      start += 1
    }
    maxSum
  }

}

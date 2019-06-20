package algorithms.sorting

object MedianOfTwoSortedArrays {

  def median(arr1: Array[Int], arr2: Array[Int]): Double = {
    val merged = merge(arr1, arr2)

    if (merged.length % 2 == 0) {
      val index = merged.length / 2
      val v1 = merged(index - 1)
      val v2 = merged(index)
      (v1 + v2) / 2D
    } else merged(merged.length / 2)
  }

  private def merge(arr1: Array[Int], arr2: Array[Int], memo: Array[Int] = Array.empty[Int]): Array[Int] = {
    (arr1, arr2) match {
      case (Array(), Array()) => memo
      case (Array(), Array(h, t@_*)) => merge(arr1, t.toArray, h +: memo)
      case (Array(h, t@_*), Array()) => merge(t.toArray, arr2, h +: memo)
      case (Array(h1, t1@_*), Array(h2, t2@_*)) =>
        if (h1 < h2) merge(t1.toArray, arr2, h1 +: memo)
        else merge(arr1, t2.toArray, h2 +: memo)
    }
  }

}

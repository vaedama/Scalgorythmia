package algorithms.sorting

object MedianOfTwoSortedArrays {

  // Intuition: merge the sorted arrays
  // If the merged array length is even, we end up with two middle elements
  // Calculate the average in case of even length array.
  // Otherwise, the middle element of the array is the median
  def median(arr1: Array[Int], arr2: Array[Int]): Double = {
    val merged = MergeSortedSequences.merge(arr1, arr2)

    if (merged.length % 2 == 0) {
      val index = merged.length / 2
      val mid1 = merged(index - 1)
      val mid2 = merged(index)
      (mid1 + mid2) / 2D
    } else merged(merged.length / 2)
  }

}

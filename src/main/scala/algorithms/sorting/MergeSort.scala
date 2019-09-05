package algorithms.sorting

object MergeSort {

  /**
    * @param input sequence to be sorted
    * @param ord   sorting strategy
    * @tparam A element type
    * @return sorted sequence
    */
  def sort[A](input: Seq[A])(implicit ord: Ordering[A]): Seq[A] = {
    if (input.size <= 1) input
    else {
      val (left, right) = input.splitAt(input.size / 2)
      MergeSortedSequences.merge(sort(left), sort(right))
    }
  }

}

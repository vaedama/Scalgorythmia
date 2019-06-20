package algorithms.sorting

import scala.annotation.tailrec

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
      merge(sort(left), sort(right))
    }
  }

  private def merge[A](left: Seq[A], right: Seq[A])(implicit ord: Ordering[A]): Seq[A] = {
    @tailrec def loop(remLeft: Seq[A], remRight: Seq[A], memo: Seq[A] = Nil): Seq[A] = (remLeft, remRight) match {
      case (Seq(), Seq(h, t@_*)) =>
        loop(remLeft, t, h +: memo)
      case (Seq(h, t@_*), Seq()) =>
        loop(t, remRight, h +: memo)
      case (Seq(lh, lt@_*), Seq(rh, rt@_*)) =>
        if (ord.lt(lh, rh)) loop(lt, remRight, lh +: memo)
        else loop(remLeft, rt, rh +: memo)
      case _ =>
        memo
    }

    loop(left, right).reverse
  }
}

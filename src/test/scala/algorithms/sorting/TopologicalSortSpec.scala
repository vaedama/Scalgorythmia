package algorithms.sorting

import algorithms.sorting.TopologicalSort.CycleDetectedError
import org.scalatest.FunSuite

class TopologicalSortSpec extends FunSuite {

  test("case 1") {
    val actual = TopologicalSort.sort(Seq((3, 2), (3, 0), (2, 0), (2, 1))).right.get.result
    val expected = Seq(3, 2, 0, 1)
    assert(expected == actual)
  }

  /*
  Explanation:
  ===========
  3--->2--->1
  |    |
  |    |
  v    |
  0<---

  predecessorsByVertex = Map(
    3 -> Set(),
    2 -> Set(3),
    1 -> Set(2),
    0 -> Set(3,2)
  )
  noPreds = Map(3 -> Set())
  hasPreds = Map(2 -> Set(3), 1 -> Set(2), 0 -> Set(3,2))
  sources = Iterable(3)
  memo = Seq(3)

  predecessorsByVertex = Map(
    2 -> Set(),
    1 -> Set(2),
    0 -> Set(2)
  )
  noPreds = Map(2 -> Set())
  hasPreds = Map(1 -> Set(2), 0 -> Set(2))
  sources = Iterable(2)
  memo = Seq(3,2)

  predecessorsByVertex = Map(
    1 -> Set(),
    0 -> Set()
  )
  noPreds = Map(1 -> Set(), 0 -> Set(0))
  hasPreds = Map()
  sources = Iterable(0, 1)
  memo = Seq(3,2,0,1)
   */

  test("case 2") {
    val actual = TopologicalSort.sort(Seq((4, 2), (4, 3), (2, 0), (2, 1), (3, 1))).right.get.result
    val expected = Seq(4, 2, 3, 0, 1)
    assert(expected == actual)
  }

  /*
          -----------------------------
          |                            |
          |                            |
  6 ----> 4 <----- 5 ----> 3 ----> 0   |
  |                       | |          |
  |                       | |          |
  |                       | |          |
  v                       | v          |
  2<----------------------  1<---------

   */

  test("case 3") {
    val actual = TopologicalSort.sort(Seq((6, 4), (6, 2), (5, 3), (5, 4), (3, 0), (3, 1), (3, 2), (4, 1))).right.get.result
    val expected = Seq(5, 6, 3, 4, 0, 1, 2)
    assert(expected == actual)
  }

  /*
    ------------
    |          |
    v          |
    3--->2---->1
    |    |
    |    |
    v    |
    0<---
   */

  test("case 4") {
    val actual = TopologicalSort.sort(Seq((3, 2), (3, 0), (2, 0), (2, 1), (1, 3))).left.get
    val expected = CycleDetectedError(Map(3 -> Set(3, 2, 1)))
    assert(expected == actual)
  }

}

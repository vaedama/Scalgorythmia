package datastructures.immutable.list

import org.scalatest.FunSuite

class ListSpec extends FunSuite {

  private val list = List(1, 2, 3)

  test("head") {
    assert(list.head == 1)
  }

  test("tail") {
    assert(list.tail == List(2, 3))
  }

  test("prepend") {
    assert(list.prepend(0) == List(0, 1, 2, 3))
  }

  test("append") {
    assert(list.append(4) == List(1, 2, 3, 4))
  }

  test("reverse") {
    assert(list.reverse == List(3, 2, 1))
  }

  test("lift: Some case") {
    assert(list.lift(2).contains(3))
  }
  test("lift: None case ") {
    assert(list.lift(100).isEmpty)
  }

  test("drop -1") {
    assert(list.drop(-1) == list)
  }
  test("drop 0") {
    assert(list.drop(0) == list)
  }
  test("drop 1") {
    assert(list.drop(1) == List(2, 3))
  }
  test("drop 100") {
    assert(list.drop(100) == Nil)
  }

  test("dropWhile") {
    assert(list.dropWhile(_ <= 2) == List(3))
  }

  test("take -1") {
    assert(list.take(-1) == Nil)
  }
  test("take 0") {
    assert(list.take(0) == Nil)
  }
  test("take 1") {
    assert(list.take(1) == List(1))
  }
  test("take 100") {
    assert(list.take(100) == list)
  }

  test("takeWhile") {
    assert(list.takeWhile(_ <= 2) == List(1, 2))
  }

  test("size") {
    assert(list.size == 3)
  }

  test("updated") {
    assert(list.updated(1, 100) == List(1, 100, 3))
  }

  test("remove") {
    assert(list.remove(2) == List(1, 3))
  }

  test("sum") {
    assert(list.sum == 6)
  }

  test("product") {
    assert(list.product == 6)
  }

}

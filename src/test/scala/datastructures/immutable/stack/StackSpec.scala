package datastructures.immutable.stack

import org.scalatest.FunSuite

class StackSpec extends FunSuite {

  private val stack = Stack.empty[Int]
    .push(1)
    .push(2)
    .push(3)
    .push(4)

  test("pop") {
    val (top, bottom) = stack.pop
    assert(top.contains(4))
    assert(bottom == Stack(List(3, 2, 1)))
  }

  test("push") {
    assert(stack.push(5) == Stack(List(5, 4, 3, 2, 1)))
  }

  test("top") {
    assert(stack.top.contains(4))
  }

  test("rest") {
    assert(stack.bottom == Stack(List(3, 2, 1)))
  }

  test("isEmpty") {
    assert(!stack.isEmpty)
  }

  test("toList") {
    assert(stack.toList == List(4, 3, 2, 1))
  }

  test("size") {
    assert(stack.size == 4)
  }

}

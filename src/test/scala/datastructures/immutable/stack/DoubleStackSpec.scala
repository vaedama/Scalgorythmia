package datastructures.immutable.stack

import org.scalatest.FunSuite

class DoubleStackSpec extends FunSuite {

  private val stack = DoubleStack.empty[Int]
    .push1(1)
    .push2(2)
    .push1(3)
    .push2(4)

  //[
  //  3,
  //  1,
  //  2,
  //  4
  //]

  test("pop1") {
    val (top, bottom) = stack.pop1
    assert(top.contains(3))
    assert(bottom == DoubleStack(Vector(1, 2, 4)))
  }

  test("pop2") {
    val (top, bottom) = stack.pop2
    assert(top.contains(4))
    assert(bottom == DoubleStack(Vector(3, 1, 2)))
  }

  test("push1") {
    assert(stack.push1(5) == DoubleStack(Vector(5, 3, 1, 2, 4)))
  }

  test("push2") {
    assert(stack.push2(5) == DoubleStack(Vector(3, 1, 2, 4, 5)))
  }

  test("isEmpty") {
    assert(!stack.isEmpty)
  }

  test("toList") {
    assert(stack.toList == List(3, 1, 2, 4))
  }

  test("size") {
    assert(stack.size == 4)
  }

}

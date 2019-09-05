package datastructures.immutable.stack

import datastructures.immutable.queue.Queue
import org.scalatest.FunSuite

class StackSpec extends FunSuite {

  private val stack = Stack.empty[Int]
    .push(1)
    .push(2)
    .push(3)
    .push(4)

  test("==") {
    val stack2 = Stack(1, 2, 3, 4)
    assert(stack === stack2)
  }

  test("pop") {
    val Some((top, bottom)) = stack.pop
    assert(top == 4)
    assert(bottom === Stack(1, 2, 3))
  }

  test("push") {
    assert(stack.push(5) === Stack(1, 2, 3, 4, 5))
  }

  test("top") {
    assert(stack.top.contains(4))
  }

  test("bottom") {
    assert(stack.bottom === Stack(1, 2, 3))
  }

  test("isEmpty") {
    assert(!stack.isEmpty)
  }

  test("size") {
    assert(stack.size == 4)
  }

  test("toList") {
    assert(stack.toList === List(4, 3, 2, 1))
  }

}

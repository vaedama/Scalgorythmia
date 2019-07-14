package datastructures.immutable.stack

import org.scalatest.FreeSpec

class MinMaxStackSpec extends FreeSpec {

  "A MinMaxStack" - {
    val stack = MinMaxStack.empty[Int]

    "when empty should have size 0" in {
      assert(stack.size == 0)
    }

    "should produce None when pop is invoked" in {
      assert(stack.pop === None)
    }

    "should produce new MinMaxStack when push is invoked" in {
      assert(stack.push(5) === MinMaxStack(5))
    }

    "should produce min on a non-empty stack when min is invoked" in {
      assert(stack.push(5).min === Some(5))
      assert(stack.push(5).push(7).min === Some(5))
    }

    "should produce max on a non-empty stack when max is invoked" in {
      assert(stack.push(5).max === Some(5))
      assert(stack.push(5).push(7).max === Some(7))
    }

    "should produce top on a non-empty stack when top is invoked" in {
      assert(stack.push(5).top === Some(5))
      assert(stack.push(5).push(7).top === Some(7))
    }

  }
}

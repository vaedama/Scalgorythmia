package datastructures.immutable.queue

import org.scalatest.FunSuite

class QueueSpec extends FunSuite {

  private val queue = Queue.empty[Int]
    .enqueue(1)
    .enqueue(2)
    .enqueue(3)
    .enqueue(4)

  test("enqueue") {
    assert(queue.enqueue(5) === Queue(List(5, 4, 3, 2, 1), Nil, 5))
  }

  test("dequeue") {
    val (frontOption, rearQ) = queue.dequeue
    assert(frontOption.contains(1))
    assert(rearQ === Queue(Nil, List(2, 3, 4), 3))
  }

  test("front") {
    assert(queue.front.contains(1))
  }

  test("rear") {
    assert(queue.rear === Queue(Nil, List(2, 3, 4), 3))
  }

  test("isEmpty") {
    assert(!queue.isEmpty)
  }

  test("size") {
    assert(queue.size === 4)
  }

  test("toList") {
    assert(queue.toList === List(1, 2, 3, 4))
  }

  test("reverse") {
    assert(queue.reverse(2) === Queue(Nil, List(2, 1, 3, 4), 4))
    assert(queue.reverse(100) === Queue(Nil, List(4, 3, 2, 1), 4))
  }

}

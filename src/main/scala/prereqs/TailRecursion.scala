package prereqs

import util.Profiler

import scala.annotation.tailrec

object TailRecursion {

  /**
    * @param n a non-negative integer
    * @return factorial of n
    */
  def factorial(n: BigInt): BigInt = {
    @tailrec def loop(rem: BigInt, memo: BigInt = 1): BigInt =
      if (rem <= 1) memo
      else loop(rem - 1, memo * rem)

    loop(n)
  }

  /**
    * Notes about Tail Recursion:
    *
    * Q) Why do we need recursion?
    * We don't need recursion. We need clarity.
    * Recursion naturally aides readability compared to iteration.
    *
    * Q) What is @tailrec
    * Annotating with @tailrec ensures that the tail call will be optimized by the Scala compiler.
    * If the tail call cannot be optimized, we get nice compile time error from the compiler.
    *
    * Q) What is tail call? How does it get optimized?
    * Tail call means last call of the recursive method.
    * Tail call optimization means having the ability to avoid allocating new stack frame for a function.
    *
    * Q) How is that possible?
    * Because the calling function will simply return the value that it gets from the called function.
    * We keep track of the new data for every call to the recursive function.
    *
    * If we were to call factorial(1000000), we'd only need the same amount of space as factorial(5)
    * This is not the case with non-tail-recursive factorial and such large values may cause stack overflow.
    */

  // Enrich BigInt type to support unary exclamation mark operator for factorial
  implicit class BigIntFactorial(n: BigInt) {
    def ! : BigInt = factorial(n)
  }

}

object TailRecursionDemo extends App {

  Profiler.profile("Factorial 0") {
    println(s"0! = ${TailRecursion.factorial(0)}")
  }

  Profiler.profile("Factorial 1") {
    println(s"1! = ${TailRecursion.factorial(1)}")
  }

  Profiler.profile("Factorial 5") {
    println(s"5! = ${TailRecursion.factorial(5)}")
  }

  Profiler.profile("Factorial 100000") {
    println(s"100000! = ${TailRecursion.factorial(100000)}")
  }

  Profiler.profile("100000!") {
    import TailRecursion._
    println(s"100000! = ${BigInt(100000) !}")
  }

}
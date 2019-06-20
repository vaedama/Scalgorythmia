package prereqs

import util.Profiler

import scala.collection.mutable

object Memoization {

  // Slow performing since it has to compute previously computed values
  def recursiveFibonacci(n: Int): BigInt = {
    if (n == 0) 0
    else if (n == 1) 1
    else recursiveFibonacci(n - 1) + recursiveFibonacci(n - 2)
  }

  case class Memo[I, O](f: I => O) extends mutable.HashMap[I, O] {
    override def apply(in: I): O = getOrElseUpdate(in, f(in))
  }

  // Trades time complexity with space complexity
  // Note that this class-level val compiles to a combination of a method and a private variable
  // Hence the recursive definition is allowed
  val memoizedFibonacci: Int => BigInt = Memo {
    case 0 => 0
    case 1 => 1
    case n => memoizedFibonacci(n - 1) + memoizedFibonacci(n - 2)
  }

  // lazy would come handy if we cannot write this val at class level
  // This version wastes extra CPU cycles with lazy lock checking
  lazy val lazyMemoizedFibonacci: Int => BigInt = Memo {
    case 0 => 0
    case 1 => 1
    case n => lazyMemoizedFibonacci(n - 1) + lazyMemoizedFibonacci(n - 2)
  }

  // Careful: This creates new function backed by new map each time
  // Hence this is the worst performing of all
  def incorrectMemoizationFib: Int => BigInt = Memo {
    case 0 => 0
    case 1 => 1
    case n => incorrectMemoizationFib(n - 1) + incorrectMemoizationFib(n - 2)
  }

}

object MemoizationDemo extends App {

  Profiler.profile("First 40 fibonacci sequence using recursion") {
    println((1 to 40).map(Memoization.recursiveFibonacci))
  }

  Profiler.profile("First 40 fibonacci sequence using memoized val") { // O(log n)
    println((1 to 40).map(Memoization.memoizedFibonacci))
  }

  Profiler.profile("First 40 fibonacci sequence using memoized lazy val") {
    println((1 to 40).map(Memoization.lazyMemoizedFibonacci))
  }

  Profiler.profile("First 40 fibonacci sequence using memoized def") {
    println((1 to 40).map(Memoization.incorrectMemoizationFib))
  }

}

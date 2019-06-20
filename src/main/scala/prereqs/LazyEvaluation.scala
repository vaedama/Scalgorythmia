package prereqs

object LazyEvaluation {

  def findOdd(n: Int): Option[Int] = {
    println(s"findOdd = $n")
    if (n % 2 != 0) Some(n) else None
  }

  def findGte5(n: Int): Option[Int] = {
    println(s"findGte5 = $n")
    if (n >= 5) Some(n) else None
  }

}

object LazyEvaluationDemo extends App {

  locally {
    val input = (5 to 10).toList
    val output = input
      .flatMap(LazyEvaluation.findOdd)
      .flatMap(LazyEvaluation.findGte5)

    println(s"output=$output")
  }

  locally {
    val lazyInput = (5 to 10).view
    val lazyOutput = lazyInput
      .flatMap(LazyEvaluation.findOdd)
      .flatMap(LazyEvaluation.findGte5)

    println("Forcing lazy output...")

    /*
    Notice how first flatMap doesn't generate a full list before moving to second flatMap
    All checks are happening at the same time thus copying is elided.
     */
    lazyOutput.force
  }


}


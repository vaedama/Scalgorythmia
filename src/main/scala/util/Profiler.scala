package util

object Profiler {

  def profile[R](operation: String)(block: => R): R = {
    val t0 = System.nanoTime
    val result = block // call-by-name
    val t1 = System.nanoTime
    println(s"$operation - elapsed time: ${t1 - t0} ns")
    result
  }

}

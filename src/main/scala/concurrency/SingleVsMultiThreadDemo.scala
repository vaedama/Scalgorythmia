package concurrency

object SingleVsMultiThreadDemo extends App {

  locally {
    val range = 0 to 100 * 100
    singleThreadDemo(range)
    multiThreadDemo(range)
  }

  def singleThreadDemo(range: Range): Unit = {
    val single = new Thread(() => range.filter(_ % 2 == 0))

    profile("SingleThreaded") {
      single.start()
      single.join()
    }
  }

  def multiThreadDemo(range: Range): Unit = {
    val (range1, range2) = range.splitAt(range.size / 2)

    val thread1 = new Thread(() => range1.filter(_ % 2 == 0))
    val thread2 = new Thread(() => range2.filter(_ % 2 == 0))

    profile("MultiThreaded") {
      // Run both threads
      thread1.start()
      thread2.start()

      // Wait for both threads to complete
      thread1.join()
      thread2.join()
    }
  }

  def profile[A](operation: String)(f: => A): Unit = {
    val start = System.currentTimeMillis
    f
    println(s"Latency of operation: $operation = ${System.currentTimeMillis - start}")
  }

}


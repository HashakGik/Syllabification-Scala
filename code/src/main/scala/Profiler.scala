import java.lang.System.nanoTime

/**
 * Trait implementing a simple profiler.
 */
trait Profiler {
  /**
   * Executes a function and measures the time (in nano seconds) it takes to execute it.
   * @param f Code to profile.
   * @tparam T Return type of f.
   * @return Tuple (function output, time in nano seconds).
   */
  def profile[T](f: => T): (T, Long) = { // Call by name to execute the callback only when its time.
    val t0: Long = nanoTime()
    val out: T = f
    val t1: Long = nanoTime()

    (out, t1 - t0)
  }

  /**
   * Repeats the execution of a function multiple times and averages its running time.
   * @param f Code to profile.
   * @param times Number of times to re-execute the code.
   * @tparam T Return type of f.
   * @return Tuple (function output at the last execution, time in nano seconds averaged over times re-executions).
   */
  def smoothProfile[T](f: => T, times: Int = 100): (T, Long) = {
    var avg: Long = 0
    var out: (T, Long) = (null.asInstanceOf[T], 0)
    for (i <- 0 until times) {
      out = profile(f)
      avg += out._2
    }

    (out._1, avg / times)
  }
}

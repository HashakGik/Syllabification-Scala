import java.io.{File, PrintWriter}
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

/**
 * Program entrypoint.
 */
object Main {
  /**
   * Main method.
   * @param args Command line arguments.
   */
  def main(args: Array[String]): Unit = {
    val timeFormatter: DateTimeFormatter = DateTimeFormatter.ofPattern("dd-MM-yyyy HH:mm:ss")
    
    val profileLog: PrintWriter = new PrintWriter(new File("profiling_using_queues.log"))

    profileLog.write("################################\r\n")
    profileLog.write("Evaluating the performance with respect to the original dataset.\r\n")
    profileLog.write("################################\r\n")
    println("[" + timeFormatter.format(LocalDateTime.now()) + "] Sequential run, normal file...")
    SequentialRun("La Divina Commedia.txt", "outSeq.txt", "statsSeq.txt", profileLog, 4, 50, true, 5)
    println("[" + timeFormatter.format(LocalDateTime.now()) + "] Parallel run, normal file...")
    ParallelRun("La Divina Commedia.txt", "outPar.txt", "statsPar.txt", profileLog, 4, 50, true, 5)
    println("[" + timeFormatter.format(LocalDateTime.now()) + "] Spark 1 run, normal file...")
    SparkRun1("La Divina Commedia.txt", "outSpark1.txt", "statsSpark1.txt", profileLog, 4, 50, true)
    println("[" + timeFormatter.format(LocalDateTime.now()) + "] Spark 2 run, normal file...")
    SparkRun2("La Divina Commedia.txt", "outSpark2.txt", "statsSpark2.txt", profileLog, 50, true)


    // Dataset copypasted 5 times.
    profileLog.write("################################\r\n")
    profileLog.write("Evaluating the performance with respect to a large file (original dataset repeated 5 times).\r\n")
    profileLog.write("################################\r\n")
    println("[" + timeFormatter.format(LocalDateTime.now()) + "] Sequential run, long file...")
    SequentialRun("La Divina Commedia large.txt", "", "", profileLog, 20, 50, true, 1)
    println("[" + timeFormatter.format(LocalDateTime.now()) + "] Parallel run 4 threads, long file...")
    ParallelRun("La Divina Commedia large.txt", "", "", profileLog, 4, 50, true, 1) // Number of cores in the machine.
    println("[" + timeFormatter.format(LocalDateTime.now()) + "] Parallel run 20 threads, long file...")
    ParallelRun("La Divina Commedia large.txt", "", "", profileLog, 20, 50, true, 1) // Investigating the overhead of many context switches.
    println("[" + timeFormatter.format(LocalDateTime.now()) + "] Spark 1 run 4 nodes, long file...")
    SparkRun1("La Divina Commedia large.txt", "", "", profileLog, 4, 50, true) // Number of cores in the machine.
    println("[" + timeFormatter.format(LocalDateTime.now()) + "] Spark 1 run 20 nodes, long file...")
    SparkRun1("La Divina Commedia large.txt", "", "", profileLog, 20, 50, true) // Investigating the overhead.
    println("[" + timeFormatter.format(LocalDateTime.now()) + "] Spark 2 run, long file...")
    SparkRun2("La Divina Commedia large.txt", "", "", profileLog, 50, true)
    println("[" + timeFormatter.format(LocalDateTime.now()) + "] Done...")

    // Dataset copypasted 10 times. Only the tests which scaled relatively well are run.
    profileLog.write("################################\r\n")
    profileLog.write("Evaluating the performance with respect to a very large file (original dataset repeated 10 times).\r\n")
    profileLog.write("################################\r\n")
    println("[" + timeFormatter.format(LocalDateTime.now()) + "] Sequential run, huge file...")
    SequentialRun("La Divina Commedia huge.txt", "", "", profileLog, 20, 50, true, 1)
    println("[" + timeFormatter.format(LocalDateTime.now()) + "] Parallel run 20 threads, huge file...")
    ParallelRun("La Divina Commedia huge.txt", "", "", profileLog, 20, 50, true, 1) // Number of cores in the machine.
    println("[" + timeFormatter.format(LocalDateTime.now()) + "] Spark 1 run 20 nodes, huge file...")
    SparkRun1("La Divina Commedia huge.txt", "", "", profileLog, 20, 50, true) // Investigating the overhead.
    println("[" + timeFormatter.format(LocalDateTime.now()) + "] Done...")

    // Investigating the effects of the number of threads/nodes.
    profileLog.write("################################\r\n")
    profileLog.write("Evaluating the performance with respect to the number of threads\r\n")
    profileLog.write("################################\r\n")

    for (i <- 1 to 50) {
      println("[" + timeFormatter.format(LocalDateTime.now()) + "] Parallel run " + i * 2 + " threads, normal file...")
      ParallelRun("La Divina Commedia.txt", "", "", profileLog, i * 2, 50, true, 5)
    }

    profileLog.close()

    // TODO: BUG: spark versions add # in the space between pages!
  }
}
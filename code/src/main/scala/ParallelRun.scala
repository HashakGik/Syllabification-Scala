import java.io.{File, PrintWriter}

import syllabification.{ChunkSplitter, Page, WordSplitter}

import scala.collection.immutable.Queue

/**
 * Case class for the parallel run. The input is split into chunks, each of them is processed by a different thread and the result is finally merged.
 * Execution is split into four phases in order to profile each operation separately.
 * @param in Input filename.
 * @param out Output filename for the syllabified text.
 * @param stats Output filename for the statistics on syllable occurrences.
 * @param profileLog File handler for the profiling log.
 * @param chunks Number of chunks in which the input will be split.
 * @param lookahead Number of lookahead characters to search for each splitting point.
 * @param profiling If true profiles the execution and saves the result on profileLog.
 * @param times Number of times the execution will be repeated to improve profiling precision.
 */
case class ParallelRun(in: String, out: String = "", stats: String = "", profileLog: PrintWriter, chunks: Int = 10, lookahead: Int = 50, profiling: Boolean = false, times: Int) extends Profiler {
  if (profiling) {
    profileLog.write("Profiling the parallel execution on file \"" + in + "\", split into " + chunks + " chunks (profiling averaged over " + times + " runs). Each chunk is processed by a different thread...\r\n")
    val ((pages, file), t1): ((Queue[Page], PrintWriter), Long) = smoothProfile(init(in, out, chunks, lookahead), times)
    val (threads, t2): (Array[Thd], Long) = smoothProfile(splitSyllables(pages, file), times)
    val (syllableCount, t3): (Map[String, Int], Long) = smoothProfile(merge(threads), times)
    val (_, t4): (Unit, Long) = smoothProfile(extractStats(syllableCount, stats), times)
    val total = t1 + t2 + t3 + t4
    profileLog.write("Total (averaged) running time: " + total / 1e6 + " ms, of which:\r\n")
    profileLog.write("\t" + t1 / 1e6 + " ms for reading the file and splitting into chunks (sequential),\r\n")
    profileLog.write("\t" + t2 / 1e6 + " ms for splitting each word into syllables and building a dictionary of occurrences for each chunk (parallel),\r\n")
    profileLog.write("\t" + t3 / 1e6 + " ms for merging the dictionaries into one (sequential),\r\n")
    profileLog.write("\t" + t4 / 1e6 + " ms for sorting and extracting the centiles (sequential).\r\n")
    profileLog.write("\r\n")
    profileLog.flush()
  }
  else {
    val (pages, file) = init(in, out, chunks, lookahead)
    val threads: Array[Thd] = splitSyllables(pages, file)
    val syllableCount: Map[String, Int] = merge(threads)
    extractStats(syllableCount, stats)
  }

  /**
   * Thread class processing a single page.
   * @param page Page associated to the thread.
   */
  class Thd(page: Page) extends Thread {
    var result: Map[String, Int] = _
    var syllabifiedString: String = ""

    /**
     * Thread's code. Equivalent to a single iteration of [[SequentialRun.splitSyllables]].
     * @note Since it's an [[https://en.wikipedia.org/wiki/Embarrassingly_parallel Embarassingly parallel]] problem, no form of synchronization between threads is required.
     */
    override def run(): Unit = {
      val ws = new WordSplitter(page)
      val partialCount = ws.getWords().
        flatMap(w => w.toQueueOfSyllables()).
        map(syl => syl.toPrettyString()).
        map(str => (str, 1)).
        groupBy(t => t._1).
        mapValues(_.map(_._2).sum)

      this.syllabifiedString = ws.getWords().map(w => w.toSyllabifiedString()).mkString("#") // Output to file must be postponed after the join, otherwise chunks may be saved out of order.
      this.result = partialCount
    }
  }

  /**
   * Reads the input file into memory and splits it into pages.
   * @param in Input filename.
   * @param out Output filename.
   * @param chunks Number of chunks in which to split the input.
   * @param lookahead Number of lookahead characters.
   * @return Queue of pages and filehandler to the output file.
   */
  private def init(in: String, out: String, chunks: Int, lookahead: Int): (Queue[Page], PrintWriter) = {
    val pages: Queue[Page] = ChunkSplitter.loadFile(in, chunks, lookahead)
    val file: PrintWriter = if (out != "") new PrintWriter(new File(out)) else null

    (pages, file)
  }

  /**
   * Creates a thread for each page, waits to collect the syllabified results and finally saves the syllabified text to output.
   * @param pages Queue of pages.
   * @param file Output file handler.
   * @return Queue of partial occurrence counts.
   */
  private def splitSyllables(pages: Queue[Page], file: PrintWriter): Array[Thd] = {
    var totalCount: Queue[Map[String, Int]] = Queue()
    val threads: Array[Thd] = new Array[Thd](pages.length)
    for (i <- pages.indices) {
      threads(i) = new Thd(pages(i))
      threads(i).start()
    }

    for (i <- pages.indices) {
      threads(i).join()
    }

    for (i <- pages.indices) {
      // Since each thread is ordered, there is no need to check the pages' id.
      if (file != null) {
        file.write(threads(i).syllabifiedString)
        file.flush()
      }
    }

    if (file != null) {
      file.close()
    }

    threads
  }

  /**
   * Merges each partial count into a single map.
   * @param threads Array of threads, each encapsulating the partial count.
   * @return Single map of occurrences.
   */
  private def merge(threads: Array[Thd]): Map[String, Int] = {
    threads.map(thd => thd.result).reduce(
      (m1, m2) => m1 ++ m2.map {
        case (k, v) => k -> (v + m1.getOrElse(k, 0))
      }
    )
  }

  /**
   * Sorts and extracts descriptive statistics (centiles) from the map of occurrences.
   * @param syllableCount Map of occurrences.
   * @param stats Output filename for the statistics.
   */
  private def extractStats(syllableCount: Map[String, Int], stats: String = ""): Unit = {
    val file: PrintWriter = if (stats != "") new PrintWriter(new File(stats)) else null
    val sortedCount = syllableCount.toSeq.sortBy(t => -t._2)

    if (file != null) file.write("Maximum frequency syllable: " + sortedCount.head + "\r\n")
    for (i <- 1 until 100) {
      if (file != null) file.write(i + "th centile: " + sortedCount(sortedCount.length * i / 100 - 1) + "\r\n")
    }
    if (file != null) {
      file.write("Minimum frequency syllable: " + sortedCount.last + "\r\n")
      file.close()
    }
  }
}
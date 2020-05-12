import java.io.{File, PrintWriter}

import syllabification.{ChunkSplitter, Page, WordSplitter}

import scala.collection.immutable.Queue

/**
 * Case class for the sequential run. The input is split into chunks and each of them is processed in a for loop.
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
case class SequentialRun(in: String, out: String = "", stats: String = "", profileLog: PrintWriter, chunks: Int = 10, lookahead: Int = 50, profiling: Boolean = false, times: Int) extends Profiler {
  if (profiling) {
    profileLog.write("Profiling the sequential execution on file \"" + in + "\", split into " + chunks + " chunks (profiling averaged over " + times + " runs)...\r\n")
    val ((pages, file), t1): ((Queue[Page], PrintWriter), Long) = smoothProfile(init(in, out, chunks, lookahead), times)
    val (syllableDicts, t2): (Queue[Map[String, Int]], Long) = smoothProfile(splitSyllables(pages, file), times)
    val (syllableCount, t3): (Map[String, Int], Long) = smoothProfile(merge(syllableDicts), times)
    val (_, t4): (Unit, Long) = smoothProfile(extractStats(syllableCount, stats), times)
    val total = t1 + t2 + t3 + t4
    profileLog.write("Total (averaged) running time: " + total / 1e6 + " ms, of which:\r\n")
    profileLog.write("\t" + t1 / 1e6 + " ms for reading the file and splitting into chunks,\r\n")
    profileLog.write("\t" + t2 / 1e6 + " ms for splitting each word into syllables and building a dictionary of occurrences for each chunk,\r\n")
    profileLog.write("\t" + t3 / 1e6 + " ms for merging the dictionaries into one,\r\n")
    profileLog.write("\t" + t4 / 1e6 + " ms for sorting and extracting the centiles.\r\n")
    profileLog.write("\r\n")
    profileLog.flush()
  }
  else {
    val (pages, file) = init(in, out, chunks, lookahead)
    val syllableDicts: Queue[Map[String, Int]] = splitSyllables(pages, file)
    val syllableCount: Map[String, Int] = merge(syllableDicts)
    extractStats(syllableCount, stats)
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
   * Splits iteratively each page into words and then syllables.
   * @param pages Queue of pages.
   * @param file Output file handler.
   * @return Queue of partial occurrence counts.
   */
  private def splitSyllables(pages: Queue[Page], file: PrintWriter): Queue[Map[String, Int]] = {
    var totalCount: Queue[Map[String, Int]] = Queue()
    for (p <- pages) {
      val ws = new WordSplitter(p)
      val partialCount = ws.getWords(). // Split words.
        flatMap(w => w.toQueueOfSyllables()). // Split syllables (flattening the queue of queues).
        map(syl => syl.toPrettyString()). // Unify different capitalizations of the same syllable.
        map(str => (str, 1)). // Create a tuple of occurrences. From this point reduceByKey needs to be manually implemented (since it's a Spark RDD transformation, not a native Scala function).
        groupBy(t => t._1). // Create a queue of queues, aggregating them by key (first element of the tuple).
        mapValues(_.map(_._2).sum) // Aggregate each subqueue by summing the values (second element of the tuple).

      if (file != null) {
        file.write(ws.getWords().map(w => w.toSyllabifiedString()).mkString("#"))
        file.flush()
      }
      totalCount = totalCount.enqueue(partialCount) // Concatenate each page into a queue of maps.
    }

    if (file != null) {
      file.close()
    }

    totalCount
  }

  /**
   * Merges each partial count into a single map.
   * @param totalCount Queue of occurrence maps.
   * @return Single map of occurrences.
   */
  private def merge(totalCount: Queue[Map[String, Int]]): Map[String, Int] = {
    totalCount.reduce(
      (m1, m2) => m1 ++ m2.map { // Concatenate two maps (NOTE: in case of duplicates ++ uses the RIGHT map's value).
        case (k, v) => k -> (v + m1.getOrElse(k, 0)) // Replace the right map's value with the sum of the two.
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
    val sortedCount = syllableCount.toSeq.sortBy(t => -t._2) // Sort the map by descending order.

    // Get descriptive statistics from the map.
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

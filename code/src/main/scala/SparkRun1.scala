import java.io.{File, PrintWriter}

import org.apache.spark.{SparkConf, SparkContext}
import org.apache.spark.rdd.RDD
import syllabification.{ChunkSplitter, Page, Syllable, Word, WordSplitter}

import scala.collection.immutable.Queue

/**
 * Case class for one of the distributed runs. The input is split into chunks, each of them is processed by a different node and the result is finally merged.
 * Execution is split into four phases in order to profile each operation separately.
 * @note Due to the lack of control over Spark's optimizations, profiling will be intrinsically less precise than [[SequentialRun]] and [[ParallelRun]],
 *       moreover, due to caching, a warm restart would cause the average of multiple runs to underestimate the actual value.
 *       For this reason, profiling in this class is performed only once.
 * @param in Input filename.
 * @param out Output filename for the syllabified text.
 * @param stats Output filename for the statistics on syllable occurrences.
 * @param profileLog File handler for the profiling log.
 * @param chunks Number of chunks in which the input will be split.
 * @param lookahead Number of lookahead characters to search for each splitting point.
 * @param profiling If true profiles the execution and saves the result on profileLog.
 */
case class SparkRun1(in: String, out: String = "", stats: String = "", profileLog: PrintWriter, chunks: Int = 10, lookahead: Int = 50, profiling: Boolean = false) extends Profiler {
  if (profiling) {
    profileLog.write("Profiling the distributed execution on file \"" + in + "\", split into " + chunks + " chunks (profiling CANNOT be averaged). Each chunk is processed by a different node...\r\n")
    val (sc, t1): (SparkContext, Long) = profile(initSpark(chunks))
    val ((splitterRDD, file), t2): ((RDD[WordSplitter], PrintWriter), Long) = profile(init(in, out, chunks, lookahead, sc))
    val (syllableCount, t3): (RDD[(String, Int)], Long) = profile(splitSyllables(splitterRDD, file))
    val (_, t4): (Unit, Long) = profile(extractStats(syllableCount, sc, stats))
    val total = t1 + t2 + t3 + t4
    profileLog.write("Total (averaged) running time: " + total / 1e6 + " ms, of which:\r\n")
    profileLog.write("\t" + t1 / 1e6 + " ms for initializing Spark,\r\n")
    profileLog.write("\t" + t2 / 1e6 + " ms for reading the file and splitting into chunks (sequential),\r\n")
    profileLog.write("\t" + t3 / 1e6 + " ms for splitting each word into syllables and building a dictionary of occurrences for each chunk (distributed),\r\n")
    profileLog.write("\t" + t4 / 1e6 + " ms for sorting and extracting the centiles and shutting down Spark (sequential).\r\n")
    profileLog.write("\r\n")
    profileLog.flush()
  }
  else {
    val sc: SparkContext = initSpark(chunks)
    val (splitterRDD, file): (RDD[WordSplitter], PrintWriter) = init(in, out, chunks, lookahead, sc)
    val syllableCount: RDD[(String, Int)] = splitSyllables(splitterRDD, file)
    extractStats(syllableCount, sc, stats)
  }

  /**
   * Initializes Spark's environment. The number of nodes is set to be equal to the number of pages to process.
   * @param chunks Number of nodes to configure.
   * @return A Spark context.
   */
  private def initSpark(chunks: Int): SparkContext = {
    val sparkConfig: SparkConf = new SparkConf()
    sparkConfig.setAppName("Syllabification")
    sparkConfig.setMaster("local[" + chunks + "]")

    new SparkContext(sparkConfig)
  }

  /**
   * Reads the input file into memory, splits it into pages and dispatches them to Spark.
   * @param in Input filename.
   * @param out Output filename.
   * @param chunks Number of chunks in which to split the input.
   * @param lookahead Number of lookahead characters.
   * @return Queue of pages and filehandler to the output file.
   */
  private def init(in: String, out: String, chunks: Int, lookahead: Int, sc: SparkContext): (RDD[WordSplitter], PrintWriter) = {
    val pages: Queue[Page] = ChunkSplitter.loadFile(in, chunks, lookahead)
    val file: PrintWriter = if (out != "") new PrintWriter(new File(out)) else null

    val splitters = pages.map(p => new WordSplitter(p))
    val splittersRDD = sc.parallelize(splitters)

    (splittersRDD, file)
  }

  /**
   * Transforms the splitters' RDD into an RDD of syllables, collects the syllabified text and creates another RDD of occurrences.
   * @param splittersRDD RDD of [[syllabification.WordSplitter]]s.
   * @param file File handler for the output.
   * @return RDD of syllable occurrences.
   */
  private def splitSyllables(splittersRDD: RDD[WordSplitter], file: PrintWriter): RDD[(String, Int)] = {
    val words: RDD[Word] = splittersRDD.flatMap(ws => ws.getWords())
    val syllables: RDD[Syllable] = words.flatMap(w => w.toQueueOfSyllables())

    val QueueOfSyllables = syllables.collect()
    if (file != null) {
      file.write(QueueOfSyllables.mkString("#"))
      file.close()
    }

    syllables.map(syl => (syl.toPrettyString(), 1)).reduceByKey(_ + _)
  }

  /**
   * Collects and sorts the syllable occurrences RDD and then extracts descriptive statistics (centiles) from it.
   * @param syllableCount RDD of syllable occurrences.
   * @param sc Spark context.
   * @param stats Output file name for the statistics.
   */
  private def extractStats(syllableCount: RDD[(String, Int)], sc: SparkContext, stats: String = ""): Unit = {
    val sortedCount = syllableCount.sortByKey(false).collect() // Sort the map by descending order and collect.
    val file: PrintWriter = if (stats != "") new PrintWriter(new File(stats)) else null
    sc.stop()

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
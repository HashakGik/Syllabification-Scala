import java.io.{File, PrintWriter}

import org.apache.spark.{SparkConf, SparkContext}
import org.apache.spark.rdd.RDD
import syllabification._

import scala.collection.immutable.Queue

/**
 * Case class for one of the distributed runs. The input is split sequentially into syllables and only their occurrence is computed in a distributed fashion.
 * Execution is split into four phases in order to profile each operation separately.
 * @note Due to the lack of control over Spark's optimizations, profiling will be intrinsically less precise than [[SequentialRun]] and [[ParallelRun]],
 *       moreover, due to caching, a warm restart would cause the average of multiple runs to underestimate the actual value.
 *       For this reason, profiling in this class is performed only once.
 * @param in Input filename.
 * @param out Output filename for the syllabified text.
 * @param stats Output filename for the statistics on syllable occurrences.
 * @param profileLog File handler for the profiling log.
 * @param lookahead Number of lookahead characters to search for each splitting point.
 * @param profiling If true profiles the execution and saves the result on profileLog.
 */
case class SparkRun2(in: String, out: String = "", stats: String = "", profileLog: PrintWriter, lookahead: Int = 50, profiling: Boolean = false) extends Profiler {
  if (profiling) {
    profileLog.write("Profiling the distributed execution on file \"" + in + "\" with no prior splitting (profiling CANNOT be averaged). The number of nodes is chosen by Spark...\r\n")
    val (sc, t1): (SparkContext, Long) = profile(initSpark())
    val ((syllablesRDD, file), t2): ((RDD[Syllable], PrintWriter), Long) = profile(init(in, out, lookahead, sc))
    val (syllableCount, t3): (RDD[(String, Int)], Long) = profile(countSyllables(syllablesRDD))
    val (_, t4): (Unit, Long) = profile(extractStats(syllableCount, sc, stats))
    val total = t1 + t2 + t3 + t4
    profileLog.write("Total (averaged) running time: " + total / 1e6 + " ms, of which:\r\n")
    profileLog.write("\t" + t1 / 1e6 + " ms for initializing Spark,\r\n")
    profileLog.write("\t" + t2 / 1e6 + " ms for reading the file, splitting into syllables and distributing the dataset (sequential),\r\n")
    profileLog.write("\t" + t3 / 1e6 + " ms for building a dictionary of occurrences (distributed),\r\n")
    profileLog.write("\t" + t4 / 1e6 + " ms for sorting, extracting the centiles and shutting down Spark (sequential).\r\n")
    profileLog.write("\r\n")
    profileLog.flush()
  }
  else {
    val sc: SparkContext = initSpark()
    val (syllablesRDD, file): (RDD[Syllable], PrintWriter) = init(in, out, lookahead, sc)
    val syllableCount: RDD[(String, Int)] = countSyllables(syllablesRDD)
    extractStats(syllableCount, sc, stats)
  }

  /**
   * Initializes Spark's environment. The choice of the number of nodes is left to Spark.
   * @return A Spark context.
   */
  private def initSpark(): SparkContext = {
    val sparkConfig: SparkConf = new SparkConf()
    sparkConfig.setAppName("Syllabification")
    sparkConfig.setMaster("local[*]")

    new SparkContext(sparkConfig)
  }

  /**
   * Reads the input file into memory, splits it into syllables and dispatches them to Spark.
   * @note Since syllabification is done sequentially, the output file is written in this phase.
   * @param in Input filename.
   * @param out Output filename.
   * @param lookahead Number of lookahead characters.
   * @return Queue of pages and filehandler to the output file.
   */
  private def init(in: String, out: String, lookahead: Int, sc: SparkContext): (RDD[Syllable], PrintWriter) = {
    val pages: Queue[Page] = ChunkSplitter.loadFile(in, 1, lookahead)
    val file: PrintWriter = if (out != "") new PrintWriter(new File(out)) else null

    val syllables: Queue[Syllable] = new WordSplitter(pages.head).getWords().flatMap(w => w.toQueueOfSyllables())

    val syllablesRDD = sc.parallelize(syllables)
    if (file != null) {
      file.write(syllables.mkString("#"))
      file.close()
    }

    (syllablesRDD, file)
  }

  /**
   * Creates an RDD of syllable occurrences.
   * @param syllablesRDD RDD of syllables.
   * @return RDD of occurrences.
   */
  private def countSyllables(syllablesRDD: RDD[Syllable]): RDD[(String, Int)] = {
    syllablesRDD.map(syl => (syl.toPrettyString(), 1)).reduceByKey(_ + _)
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
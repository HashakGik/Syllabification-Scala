package syllabification

import scala.annotation.tailrec
import scala.collection.immutable.Queue

/**
 * Class which splits a [[syllabification.Page]] into a queue of [[syllabification.Word]]s.
 * @param page Page associated with the splitter.
 */
class WordSplitter(val page: Page) extends Splitter {
  /**
   * Splits the owned page.
   * @return The queue of words contained in the page.
   */
  def getWords(): Queue[Word] = {
    val str = page.toString()
    val indexes = split(str)
    splitWords(str, 0, indexes, Queue())
  }

  /**
   * Recursively splits a string into a list of [[syllabification.Word]]s based on a list of indexes.
   * @param str Input string.
   * @param begin Starting index of the current recursion.
   * @param indexes Queue of indexes in which the split needs to be performed.
   * @param accumulator Accumulator for tail recursion.
   * @return Queue of words.
   */
  @tailrec
  private def splitWords(str: String, begin: Int, indexes: Queue[Int], accumulator: Queue[Word]): Queue[Word] = {
    if (indexes.isEmpty) {
      accumulator.enqueue(Word(str.substring(begin)))
    }
    else {
      val (head, tail): (Int, Queue[Int]) = indexes.dequeue
      val newWord = Word(str.substring(begin, head), accumulator.length)
      splitWords(str, head, tail, accumulator.enqueue(newWord))
    }
  }
}
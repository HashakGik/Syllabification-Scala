package syllabification

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.io.Source

/**
 * Singleton which splits a block of text into a series of [[syllabification.Page]]s.
 */
object ChunkSplitter extends Splitter {
  /**
   * Recursively splits a string into a queue of [[syllabification.Page]]s by looking for an acceptable space between words.
   * @param text Input string
   * @param fragmentSize Minimum size of the target page.
   * @param lookahead Number of lookahead characters in which the splitting point will be searched.
   * @param i Index of the current page.
   * @param chunks Number of chunks in which text will be split.
   * @param curPos Current position inside text.
   * @param accumulator Accumulator list for tail recursion.
   * @return Queue of pages split at word boundaries.
   */
  @tailrec
  private def splitChunk(text: String, fragmentSize: Int, lookahead: Int, i: Int, chunks: Int, curPos: Int, accumulator: Queue[Page]): Queue[Page] = {
    if (i == chunks) {
      accumulator
    }
    else {
      val begin = if (text.length() >= fragmentSize + lookahead) fragmentSize else text.length() - lookahead
      val end = if (text.length() >= fragmentSize + lookahead) fragmentSize + lookahead else text.length()

      val tmp = text.substring(begin, end)
      val splits = split(tmp)

      val chunkLength = if (text.length() >= fragmentSize + splits.head) fragmentSize + splits.head else text.length()

      splitChunk(text.substring(chunkLength), fragmentSize, lookahead, i + 1, chunks, curPos + chunkLength, accumulator.enqueue(Page(i, curPos, curPos + chunkLength, text.substring(0, chunkLength))))
    }
  }

  /**
   * Loads a list of [[syllabification.Page]]s from a string.
   * @param text Input string.
   * @param chunks Number of pages to split text into.
   * @param lookahead Number of lookahead characters.
   * @return Queue of pages.
   */
  def loadString(text: String, chunks: Int, lookahead: Int): Queue[Page] = {
    val fragmentSize: Int = text.length() / chunks
    splitChunk(text, fragmentSize, lookahead, 0, chunks, 0, Queue())
  }

  /**
   * Loads a list of [[syllabification.Page]]s from a file.
   *
   * @param file   Filename.
   * @param chunks Number of pages to split text into.
   * @param lookahead Number of lookahead characters.
   * @return Queue of pages.
   */
  def loadFile(file: String, chunks: Int, lookahead: Int): Queue[Page] = {
    val source = Source.fromFile(file)
    val text = try {
      source.mkString
    }
    finally {
      source.close()
    }

    loadString(text, chunks, lookahead)
  }
}

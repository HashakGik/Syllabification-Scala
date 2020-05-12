package syllabification

import scala.annotation.tailrec
import scala.collection.immutable.Queue

/**
 * Trait implementing between-words splitting procedures.
 * Since [[SparkRun1]] creates an RDD of splitters, it must be [[Serializable]].
 */
trait Splitter extends Serializable {

  /**
   * Scans the string from the center and returns the index of the split. -1 if not found.
   *
   * N.B: The trait's extender will take care of adding this index to the area's location inside its input string.
   * @param area Substring where the split needs to be searched.
   * @return Indexes of all the acceptable splits inside the area.
   */
  def split(area: String): Queue[Int] = {
    /**
     * Recursively compute the queue of acceptable splits.
     * @param str String to split.
     * @param curIndex Current index inside the string.
     * @param accumulator Accumulator queue.
     * @return Queue of acceptable splits.
     */
    @tailrec
    def split_recursive(str:String, curIndex: Int, accumulator: Queue[Int]): Queue[Int] = {
      val regex = """[,.;:"“”«»?—'`‘’\s]*\s+[,.;:"“”«»?—'`‘’\s]*""".r // Matches whitespaces optionally preceded/followed by punctuation/other whitecharacters.
      val subStr = str.substring(curIndex)

      regex.findFirstMatchIn(subStr) match {
        case Some(regexMatch) =>
          val start = if (regexMatch.start - 1 > 0) regexMatch.start - 1 else 0
          val end = if (regexMatch.end + 1 < subStr.length()) regexMatch.end + 1 else subStr.length()
          if (isSplitAcceptable(str.substring(curIndex).substring(start, end))) {
            split_recursive(str, curIndex + end, accumulator.enqueue(curIndex + start + 1))
          }
          else {
            split_recursive(str, curIndex + regexMatch.end, accumulator)
          }
        case None => accumulator
      }
    }

    split_recursive(area, 0, Queue())
  }

  /**
   * Determines if a candidate split is "poetically" acceptable.
   * This method is **heuristic**, and applies the following rules:
   * - A new line (end of verse) is always considered a split,
   * - An apostrophe always joins two words (e.g. *l'amico* is correctly kept together, but *de' raggi* which should be split is not),
   * - Two vowels meeting form a synalepha and are always joined (e.g. *selva oscura* is correctly kept together),
   * @param str Substring containing the last letter of a word, the first letter of the next word and all the whitespaces and punctuation in between.
   * @return True if the string can be split at character 1 (the first character after the last letter of the previous word), false otherwise.
   */
  private def isSplitAcceptable(str: String): Boolean = {
    val prev = str.charAt(0).toString()
    val next = str.charAt(str.length() - 1).toString()

    val vowel = """[AEIOUaeiouàèéìòóùÈ]"""
    val apostrophe = """.*['`‘’].*"""
    val newline = """.*\n+.*"""

    str.matches(newline) || // A new verse always splits a word.
      !(str.matches(apostrophe) && (prev.matches(vowel) || next.matches(vowel))) && // HEURISTIC: If there is an apostrophe and at least one of the surrounding letters is a vowel, it's HIGHLY PROBABLE that the words need to be joined.
      !(prev.matches(vowel) && next.matches(vowel)) // HEURISTIC: If there are two vowels it's HIGHLY PROBABLE it will be a synalepha.
    // TODO: Try to improve these heuristics.
  }
}

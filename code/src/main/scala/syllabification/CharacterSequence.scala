package syllabification

/**
 * Trait implementing utility methods for character sequences.
 * Since [[.SparkRun1]] and [[.SparkRun2]] create RDDs of [[syllabification.Word]]s and [[syllabification.Syllable]]s, it must be [[Serializable]].
 */
trait CharacterSequence extends Serializable {
  /**
   * String encapsulated by the character sequence.
   */
  val str: String

  /**
   * @return The raw string.
   */
  override def toString(): String = {
    str
  }

  /**
   * @return The string lowercased, without diacritics and without punctuation characters.
   */
  def toPrettyString(): String = {
    stripSpaces(stripPunctuation(removeDiacritics(str.toLowerCase())))
  }

  /**
   * @param str Input string.
   * @return str without witespaces.
   */
  private def stripSpaces(str: String): String = {
    """\s+""".r.replaceAllIn(str, "")
  }

  /**
   * @param str Input string.
   * @return str without punctuaction marks.
   */
  private def stripPunctuation(str: String): String = {
    """[,.;:"“”«»—'`‘’]+""".r.replaceAllIn(str, "")
  }

  /**
   * @param str Input string.
   * @return str with diacritics (accents and dieresis) removed.
   */
  private def removeDiacritics(str: String): String = {
    var out: String = str
    out = """[àä]+""".r.replaceAllIn(out, "a")
    out = """[èéë]+""".r.replaceAllIn(out, "e")
    out = """[ìï]+""".r.replaceAllIn(out, "i")
    out = """[òóö]+""".r.replaceAllIn(out, "o")
    out = """[ùü]+""".r.replaceAllIn(out, "u")

    out
  }
}

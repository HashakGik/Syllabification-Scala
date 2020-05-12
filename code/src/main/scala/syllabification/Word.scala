package syllabification

import scala.collection.immutable.Queue

/**
 * Case class encapsulating a single word.
 *
 * @note The characters #, §, {, } are used internally to process the word's syllables: to avoid conflicts, the original text MUST NOT contain them.
 * @note Due to some splits being inacceptable (@see [[syllabification.Splitter.split]]), it may store whitespaces as well.
 * @param str The stored word.
 * @param id Word id, in case of out-of-order processing, it can be used to sort the output correctly. Not used in the current implementation.
 */
case class Word(val str: String, val id: Int = 0) extends  CharacterSequence {
  /**
   * Splits the word into syllables separated by \#
   * @return Syllabified word as an hash-separed string.
   */
  def toSyllabifiedString(): String = {
    performFinalSplits(performInitialSplits(str))
  }

  /**
   * Splits the word into a queue of [[syllabification.Syllable]]s.
   * @return Syllabified word as a queue of [[[syllabification.Syllable]]s.
   */
  def toQueueOfSyllables(): Queue[Syllable] = {
    var ret: Queue[Syllable] = Queue()

    for (syl <- this.toSyllabifiedString().split("#").map(str => Syllable(str))) {
      ret = ret.enqueue(syl)
    }

    ret
  }

  /**
   * Performs a set of initial splits on:
   * - Multiple (different) consonants,
   * - Double consonants,
   * - Dieresis,
   * - Sure hiatuses.
   * @note This set of splits never happen at word boundaries, therefore there is no need to check for punctuation or whitespaces.
   * @param str The input word.
   * @return A partially syllabified (hash-separed) word.
   */
  private def performInitialSplits(str: String): String = {
    splitHiatus(splitDieresis(splitDoubleCons(splitMultipleCons(str))))
  }

  /**
   * Performs a set of final splits on the groups:
   * - contoid vocoid - contoid vocoid,
   * - vocoid - contoid vocoid
   * - vocoid - vocoid.
   * @param str A partially syllabified (hash-separed) word.
   * @return A fully syllabified word.
   */
  private def performFinalSplits(str: String): String = {
    var ret: String = str

    val cvcv = """(?i)([bcdfglmnpqrstvz][,.;:"“”«»?—'`‘’\s]*[aeiouàèéìóòùÈËÏ]+)([bcdfglmnpqrstvz]+[,.;:"“”«»?—'`‘’\s]*[aeiouàèéìóòùÈËÏ]+)"""
    ret = ret.replaceAll(cvcv, """$1#$2""")

    val vcv = """(?i)([aeiouàèéìóòùÈËÏ]+)([bcdfglmnpqrstvz]+[,.;:"“”«»?—'`‘’\s]*[aeiouàèéìóòùÈËÏ]+)"""
    ret = ret.replaceAll(vcv, """$1#$2""")

    val vv = """(?i)(?<=[aeiouàèéìóòùÈËÏ])(?=[aeiouàèéìóòùÈËÏ])"""
    ret = clumpDiphthongs(ret).replaceAll(vv, "#").split("§").mkString("")

    ret
  }

  /**
   * Splits double consonants (e.g. "mez-zo").
   * @param str Input word.
   * @return Word with hash-separed double consonants.
   */
  private def splitDoubleCons(str: String): String = {
    val doubles = """(?i)(?<=([bcdfglmnpqrstvz]))(?=$1)|(?<=c)(?=q)""" // Albeit inefficient, lookahead and lookbehind expressions perform zero-length matches.

    str.split(doubles).mkString("#")
  }

  /**
   * Splits multiple consonants (e.g. "al-to" but not "nostra").
   * All consecutive consonants are split, except:
   * - Impure s (s followed by another consonant, e.g. "st"),
   * - Mute consonant followed by liquid consonant (e.g. "cr"),
   * - Digrams (e.g. "cia")
   * - Trigrams (e.g. "glia").
   * @note A § character is added between illegal splits and then removed, therefore the input string MUST not contain it.
   * @param str Input word.
   * @return Word with hash-separed consecutive consonants.
   */
  private def splitMultipleCons(str: String): String = {
    // Except: s+cons, mute+liquid, digrams, trigrams
    val impureS = """(?i)(?<=s)(?=[bcdfghlmnpqrtvz])"""
    val muteLiquide = """(?i)(?<=[bcdgpt])(?=[lr])"""
    val digrams = """(?i)(?<=g)(?=li)|(?<=g)(?=n[aeiou])|(?<=s)(?=c[ei])|(?<=[cg])(?=h[eèéiì])|(?<=[cg])(?=i[aou])"""
    val trigrams = """(?i)(?<=g)(?=li[aou])|(?<=s)(?=ci[aou])"""

    val multiCons = """(?i)(?<=[bcdfglmnpqrstvz])(?=[bcdfglmnpqrstvz]+)"""

    var tmp = str.split(impureS).mkString("§") // Add a § to prevent split.
    tmp = tmp.split(muteLiquide).mkString("§")
    tmp = tmp.split(digrams).mkString("§")
    tmp = tmp.split(trigrams).mkString("")

    tmp = tmp.split(multiCons).mkString("#") // Perform every other split.

    tmp = tmp.split("§").mkString("") // Remove §.
    tmp
  }

  /**
   * Splits two consecutive vowels if either of them has a dieresis.
   * @param str Input word.
   * @return Word with hash-separed dieresis.
   */
  private def splitDieresis(str: String): String = {
    val dieresis = """(?i)(?<=[äëïöüËÏ])(?=[aeiou])|(?<=[aeiou])(?=[äëïöüËÏ])""" // Uppercase diacritics are NOT captured by ?i.

    str.split(dieresis).mkString("#")
  }

  /**
   * Heuristically splits hiatuses. In case of synalepha (i.e. there is a space between the two vowels), hiatuses are kept together.
   * @note Determining all possible hiatuses requires to know the position of the accent inside the word,
   *       therefore only certain (i.e. between vowels which always form a hiatus or with the accent marked) hiatuses are split.
   *       Every other case is ignored (this heuristic is reasonably good, since hiatuses are rare in Italian).
   * @param str Input word.
   * @return Word with hash-separed hiatuses.
   */
  private def splitHiatus(str: String): String = {
    val hiatus = """(?i)(?<=[aeoàèòóé])(?=[aeoàèòóé])|(?<=[rb]i)(?=[aeou])|(?<=tri)(?=[aeou])|(?<=[ìù])(?=[aeiou])"""

    str.split(hiatus).mkString("#")
  }

  /**
   * Clumps together dipthongs/triphthongs by separating their characters with § (and preventing accidental split by [[syllabification.Word.performFinalSplits]]).
   * @note The implementation internally uses { and } to perform an initial clumping, therefore the input MUST NOT contain them (nor §).
   * @param str Input word.
   * @return Word in which diphthongs and triphthongs are protected from splits by § between vowels.
   */
  private def clumpDiphthongs(str: String): String = {
    var ret = str
    val diphthong = """(?i)(i[,.;:"“”«»?—'`‘’\s]*[aeouàèéòóù]|u[,.;:"“”«»?—'`‘’\s]*[aeioàèéìòó]|[aeouàèéòóù][,.;:"“”«»?—'`‘’\s]*i|[aeàèé][,.;:"“”«»?—'`‘’\s]*u)""" // Splitting some "between-word"  diphthongs improves accuracy a little.
    val diphthongSep = """(\{.[,.;:"“”«»?—'`‘’\s]*)(.\})"""
    val triphthong = """(?i)(i[àèé]i|u[àòó]i|iu[òó])""" // Including non accented letters would consider double diphthongs (more probable than triphthongs + vowel).
    val triphthongSep = """(\{.)(.)(.\})"""

    ret = ret.replaceAll(triphthong, """{$1}""")
    ret = ret.replaceAll(triphthongSep, """$1§$2§$3""")
    ret = ret.replaceAll(diphthong, """{$1}""")
    ret = ret.replaceAll(diphthongSep, """$1§$2""")
    ret = ret.split("""[{}]""").mkString("")

    ret
  }
}
package syllabification

/**
 * Case class encapsulating a single syllable. It's just a container for a [[syllabification.CharacterSequence]].
 * @param str The stored syllable.
 */
case class Syllable(val str: String) extends CharacterSequence {
}

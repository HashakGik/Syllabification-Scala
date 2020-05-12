package syllabification

/**
 * Case class encapsulating a single page to work on.
 * @note Except for str, the other values are not used in the current implementation.
 * @param id Page id, in case of out-of-order processing, it can be used to sort the output correctly.
 * @param start Index of the first character of the page inside the original text.
 * @param end Index of the last character of the page inside the original text.
 * @param str Content of the page.
 */
case class Page(val id: Int, val start: Int, val end: Int, val str: String) extends CharacterSequence {
}

package syllabification
import org.scalatest.funsuite.AnyFunSuite

/**
 * Test suite for the correct handling of special characters.
 */
class CharacterSequenceTest extends AnyFunSuite {
  class MyCharacterSequence(val str: String) extends CharacterSequence

  test ("Pippo") {
    val cs = new MyCharacterSequence("Pippo")
    assert(cs.toPrettyString() == "pippo")
  }

  test ("Lorem ipsum") {
    val cs = new MyCharacterSequence("Lorem ipsum")
    assert(cs.toPrettyString() == "loremipsum")
  }

  test ("Nel mezzo... single line") {
    val cs = new MyCharacterSequence("Nel mezzo del cammin di nostra vita")
    assert(cs.toPrettyString() == "nelmezzodelcammindinostravita")
  }

  test ("Nel mezzo... multiline") {
    val str = """Nel mezzo del cammin di nostra vita
                |  mi ritrovai per una selva oscura,
                |  ché la diritta via era smarrita.""".stripMargin
    val cs = new MyCharacterSequence(str)
    assert(cs.toPrettyString() == "nelmezzodelcammindinostravitamiritrovaiperunaselvaoscuracheladirittaviaerasmarrita")
  }

  test ("Diacritics, multiline, multispaces") {
    val str = """  Questi non ciberà terra né peltro,
                |  ma sapïenza, amore e virtute,
                |  e sua nazion sarà tra feltro e feltro.""".stripMargin
    val cs = new MyCharacterSequence(str)
    assert(cs.toPrettyString() == "questinonciberaterranepeltromasapienzaamoreevirtuteesuanazionsaratrafeltroefeltro")
  }

}

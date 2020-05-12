package syllabification

import org.scalatest.funsuite.AnyFunSuite
import scala.collection.immutable.Queue

/**
 * Test suite for the correct syllabification of single words.
 */
class WordTest extends AnyFunSuite {
  def buildSyllableQueue(str: String): Queue[Syllable] = {
    var ret: Queue[Syllable] = Queue()

    for (w <- str.split("-")) {
      ret = ret.enqueue(Syllable(w))
    }

    ret
  }

  test("Pippo") {
    assert(Word("Pippo").toSyllabifiedString() == "Pip#po")
  }

  test("PipPo") {
    assert(Word("PipPo").toSyllabifiedString() == "Pip#Po")
  }

  test("Acclimatato") {
    assert(Word("Acclimatato").toSyllabifiedString() == "Ac#cli#ma#ta#to")
  }

  test("Entrare") {
    assert(Word("Entrare").toSyllabifiedString() == "En#tra#re")
  }

  test("Scienziato") {
    assert(Word("Scienziato").toSyllabifiedString() == "Scien#zia#to")
  }

  test("Acquitrino") {
    assert(Word("Acquitrino").toSyllabifiedString() == "Ac#qui#tri#no")
  }

  test("Soqquadro") {
    assert(Word("Soqquadro").toSyllabifiedString() == "Soq#qua#dro")
  }

  test("Intramoenia") {
    assert(Word("Intramoenia").toSyllabifiedString() == "In#tra#mo#e#nia")
  }

  test("Sclerificato") {
    assert(Word("Sclerificato").toSyllabifiedString() == "Scle#ri#fi#ca#to")
  }

  test("Schifiltoso") {
    assert(Word("Schifiltoso").toSyllabifiedString() == "Schi#fil#to#so")
  }

  test("Aiuola") {
    assert(Word("Aiuola").toSyllabifiedString() == "A#iuo#la")
  }

  test("Cucchiaio") {
    assert(Word("Cucchiaio").toSyllabifiedString() == "Cuc#chia#io")
  }

  test("Transnazionale") {
    assert(Word("Transnazionale").toSyllabifiedString() == "Tran#sna#zio#na#le")
  }

  test("Spaesato") {
    assert(Word("Spaesato").toSyllabifiedString() == "Spa#e#sa#to")
  }


  test("pietra e ’l") {
    assert(Word("pietra e ’l").toSyllabifiedString() == "pie#tra e ’l")
  }

  test("perch’ altra") {
    assert(Word("perch’ altra").toSyllabifiedString() == "per#ch’ al#tra")
  }

  test("l'amico") {
    assert(Word("l'amico").toSyllabifiedString() == "l'a#mi#co")
  }

  test("ch’i’ v’ho") {
    assert(Word("ch’i’ v’ho").toSyllabifiedString() == "ch’i’ #v’ho")
  }

  test(",\n  e ’l ") {
    assert(Word(",\n  e ’l ").toSyllabifiedString() == ",\n  e ’l ")
  }

  test(" ch’èi") {
    assert(Word(" ch’èi").toSyllabifiedString() == " ch’èi")
  }

  test ("Pippo Queue") {
    assert(Word("Pippo").toQueueOfSyllables() == buildSyllableQueue("Pip-po"))
  }

  test ("cane Queue") {
    assert(Word("cane").toQueueOfSyllables() == buildSyllableQueue("ca-ne"))
  }


}
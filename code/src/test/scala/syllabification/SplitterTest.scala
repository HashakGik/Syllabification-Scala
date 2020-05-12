package syllabification

import org.scalatest.funsuite.AnyFunSuite

import scala.collection.immutable.Queue

/**
 * Test suite for the correct splitting of spaces between words.
 */
class SplitterTest extends AnyFunSuite {
  class MySplitter() extends Splitter {

  }

  val mySplitter = new MySplitter()

  test ("Empty String") {
    assert(mySplitter.split("") == Queue())
  }

  test ("pippo") {
    assert(mySplitter.split("pippo") == Queue())
  }

  test("il cane") {
    assert(mySplitter.split("il cane") == Queue(2))
  }

  test ("vidi l'amico") {
    assert(mySplitter.split("vidi l'amico") == Queue(4))
  }

  test ("vidi l' amico") {
    assert(mySplitter.split("vidi l' amico") == Queue(4))
  }

  test ("vidi l 'amico") {
    assert(mySplitter.split("vidi l 'amico") == Queue(4))
  }

  test ("vidi l`amico") {
    assert(mySplitter.split("vidi l`amico") == Queue(4))
  }

  test ("vidi l` amico") {
    assert(mySplitter.split("vidi l` amico") == Queue(4))
  }

  test ("vidi l `amico") {
    assert(mySplitter.split("vidi l `amico") == Queue(4))
  }

  test ("vidi l’amico") {
    assert(mySplitter.split("vidi l’amico")== Queue(4))
  }

  test ("vidi l’ amico") {
    assert(mySplitter.split("vidi l’ amico") == Queue(4))
  }

  test ("vidi l ’amico") {
    assert(mySplitter.split("vidi l ’amico") == Queue(4))
  }


  test ("l'amico") {
    assert(mySplitter.split("l'amico") == Queue())
  }

  test ("l' amico") {
    assert(mySplitter.split("l' amico") == Queue())
  }

  test ("l 'amico") {
    assert(mySplitter.split("l 'amico") == Queue())
  }

  test ("l`amico") {
    assert(mySplitter.split("l`amico") == Queue())
  }

  test ("l` amico") {
    assert(mySplitter.split("l` amico") == Queue())
  }

  test ("l `amico") {
    assert(mySplitter.split("l `amico") == Queue())
  }

  test ("l’amico") {
    assert(mySplitter.split("l’amico") == Queue())
  }

  test ("l’ amico") {
    assert(mySplitter.split("l’ amico") == Queue())
  }

  test ("l ’amico") {
    assert(mySplitter.split("l ’amico") == Queue())
  }

  test("udi’ «Dolce") {
    // Not a synalepha, but it's an exception.
    assert(mySplitter.split("udi’ «Dolce") == Queue())
  }

  test("E io a lui: «Ancor vo’ che mi ’nsegni") {
    // Synalepha mi 'nsegni (metrically correct).
    // Synalepha E io a (metrically incorrect, but cannot be deterministically detected).
    assert(mySplitter.split("E io a lui: «Ancor vo’ che mi ’nsegni") == Queue(6, 18, 26))
  }

  test("Nel mezzo... one line") {
    val str = "Nel mezzo del cammin di nostra vita"
    assert(mySplitter.split(str) == Queue(3, 9, 13, 20, 23, 30))
  }

  test("Nel mezzo... no blank spaces") {
    val str = """Nel mezzo del cammin di nostra vita
                |mi ritrovai per una selva oscura,
                |ché la diritta via era smarrita.""".stripMargin
    assert(mySplitter.split(str) == Queue(3, 9, 13, 20, 23, 30, 35, 38, 47, 51, 55, 68, 73, 76, 84, 92))
  }

  test("Nel mezzo... with blank spaces") {
    val str = """Nel mezzo del cammin di nostra vita
                |  mi ritrovai per una selva oscura,
                |  ché la diritta via era smarrita.""".stripMargin
    assert(mySplitter.split(str) == Queue(3, 9, 13, 20, 23, 30, 35, 40, 49, 53, 57, 70, 77, 80, 88, 96))
  }

  test("A quella luce cotal si diventa") {
    val str = "A quella luce cotal si diventa"
    assert(mySplitter.split(str) == Queue(1, 8, 13, 19, 22))
  }
}

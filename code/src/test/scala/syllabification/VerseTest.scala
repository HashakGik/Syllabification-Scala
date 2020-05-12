package syllabification

import org.scalatest.funsuite.AnyFunSuite

/**
 * Test suite for the correct syllabification of entire verses.
 */
class VerseTest extends AnyFunSuite {

  test("Nel mezzo...") {
    val page = Page(0, 0, 0, "Nel mezzo del cammin di nostra vita")
    val ws = new WordSplitter(page)
    assert(ws.getWords().map(w => w.toSyllabifiedString()).mkString("#") == "Nel# mez#zo# del# cam#min# di# no#stra# vi#ta")
  }

  test("A quella luce...") {
    val page = Page(0, 0, 0, "A quella luce cotal si diventa")
    val ws = new WordSplitter(page)
    assert(ws.getWords().map(w => w.toSyllabifiedString()).mkString("#") == "A# quel#la# lu#ce# co#tal# si# di#ven#ta")
  }

  test("Terzina") {
    val str = """Nel mezzo del cammin di nostra vita
                |mi ritrovai per una selva oscura,
                |ché la diritta via era smarrita.""".stripMargin

    val expected = """Nel# mez#zo# del# cam#min# di# no#stra# vi#ta#
                     |mi# ri#tro#vai# per# u#na# sel#va o#scu#ra#,
                     |ché# la# di#rit#ta# via e#ra# smar#ri#ta.""".stripMargin
    val page = Page(0, 0, 0, str)
    val ws = new WordSplitter(page)
    assert(ws.getWords().map(w => w.toSyllabifiedString()).mkString("#") == expected)
  }

  test("Synalepha") {
    val str = """Tant’ è amara che poco è più morte;
                |ma per trattar del ben ch’i’ vi trovai,
                |dirò de l’altre cose ch’i’ v’ho scorte.""".stripMargin

    val expected = """Tan#t’ è a#ma#ra# che# po#co è# più# mor#te#;
                     |ma# per# trat#tar# del# ben# ch’i’ vi# tro#vai#,
                     |di#rò# de# l’al#tre# co#se# ch’i’ v’ho# scor#te.""".stripMargin
    val page = Page(0, 0, 0, str)
    val ws = new WordSplitter(page)
    assert(ws.getWords().map(w => w.toSyllabifiedString()).mkString("#") == expected)
  }

}

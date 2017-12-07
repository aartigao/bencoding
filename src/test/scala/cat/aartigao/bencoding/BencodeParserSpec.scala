package cat.aartigao.bencoding

import scala.util.parsing.input.CharSequenceReader
import BencodeParser.{Failure}

class BencodeParserSpec extends UnitSpec {

  "A BencodeParser" should "parse positive signed 64bit integers" in {
    assert(3 == BencodeParser(new CharSequenceReader("i3e")).get)
  }

  it should "parse negative signed 64bit integers" in {
    assert(-3 == BencodeParser(new CharSequenceReader("i-3e")).get)
  }

  it should "fail for negative zero integers" in {
    val res = BencodeParser(new CharSequenceReader("i-0e"))
    res shouldBe a [Failure]
  }

  it should "fail for integers starting with zero" in {
    val res = BencodeParser(new CharSequenceReader("i03e"))
    res shouldBe a [Failure]
  }

  it should "parse empty byte strings" in {
    assert("" == BencodeParser(new CharSequenceReader("0:")).get)
  }

  it should "parse byte strings" in {
    assert("spam" == BencodeParser(new CharSequenceReader("4:spam")).get)
  }

  it should "parse empty list" in {
    assert(List() == BencodeParser(new CharSequenceReader("le")).get)
  }

  it should "parse lists" in {
    assert(List("spam", "eggs") == BencodeParser(new CharSequenceReader("l4:spam4:eggse")).get)
  }

  it should "parse empty dictionary" in {
    assert(Map() == BencodeParser(new CharSequenceReader("de")).get)
  }

  it should "parse simple dictionary" in {
    assert(Map(("cow","moo"),("spam","eggs")) == BencodeParser(new CharSequenceReader("d3:cow3:moo4:spam4:eggse")).get)
  }

  it should "parse complex dictionary" in {
    assert(Map(("spam",List("a","b"))) == BencodeParser(new CharSequenceReader("d4:spaml1:a1:bee")).get)
  }

  it should "fail for invalid input" in {
    val res = BencodeParser(new CharSequenceReader("li1e4:spae"))
    res shouldBe a [Failure]
  }

}

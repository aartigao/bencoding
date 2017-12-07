package cat.aartigao.bencoding

import scala.util.parsing.combinator.{JavaTokenParsers, RegexParsers}
import scala.util.parsing.input.Reader

object BencodeParser extends RegexParsers {

  override val skipWhitespace = false

  private lazy val grammar: Parser[Any] = byteString | integer | dictionary | list | fail

  // Parser for B-encode integers
  private lazy val integer: Parser[Long] = 'i' ~> """-{0,1}(?!0)\d+""".r <~ 'e' ^^ (_.toLong)

  // Parser for B-encode byte strings
  private lazy val byteString: Parser[String] = """\d+""".r <~ ':' >> (len => s""".{$len}""".r)

  // Parser for B-encode lists
  private lazy val list: Parser[Any] = 'l' ~> rep(grammar) <~ 'e'

  // Parser for B-encode dictionaries
  private lazy val dictionary: Parser[Map[String, Any]] = 'd' ~> entries <~ 'e' ^^ (_.toMap)
  private lazy val entries = rep(byteString ~ grammar ^^ { case key ~ value => (key, value) })

  // Parser for general error messages
  private lazy val fail = failure("Unexpected token")

  def apply(in: Reader[Char]): ParseResult[Any] = parseAll(grammar, in)

}

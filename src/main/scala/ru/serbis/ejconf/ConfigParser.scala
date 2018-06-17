package ru.serbis.ejconf

import org.parboiled2.{CharPredicate, Parser, ParserInput, Rule1}


/**
  * This class contain configuration language parsing logic. In receive some,
  * string that contain input configuration, and produce AST from it.
  *
  * LIB VERSION: 0.001
  *
  */
object ConfigParser {
  trait AstNode
  case class ObjectNode(name: Option[String], body: Map[String, AstNode]) extends AstNode
  case class ArrayNode(elems: Vector[AstNode]) extends AstNode
  case class StringNode(v: String) extends AstNode
  case class IntNumberNode(v: Int) extends AstNode
  case class DoubleNumberNode(v: Double) extends AstNode
  case class BoolNode(v: Boolean) extends AstNode
}

class ConfigParser(val input: ParserInput) extends Parser {
  import ConfigParser._

  def InputLine: Rule1[Seq[ObjectNode]] = rule {
    NamedObject.+ ~ EOI
  }

  def NamedObject = rule {
    (ws.? ~ Ident ~ wsnl ~ ObjectBody ~ wsnl) ~> ((a: String, b: ObjectNode) => ObjectNode(Some(a), b.body))
  }

  def ObjectBody: Rule1[ObjectNode] = rule {
    ("{" ~ wsnl ~ ObjectMember.+ ~ wsnl ~ "}") ~> ((a: Seq[(String, AstNode)]) => ObjectNode(None, a.toMap))
  }

  def ObjectMember = rule {
    (ws.? ~ Ident ~ ws.? ~ ":" ~ ws.? ~ ObjectMemberValue ~ wsnl) ~> ((a: String, b: AstNode) => (a, b))
  }

  def ObjectMemberValue = rule {
    Bool | TripleString | SimpleString | Number | ObjectBody | Array
  }

  def Bool = rule {
    capture("true" | "false") ~> ((v: String) => BoolNode(v.toBoolean))
  }

  def SimpleString: Rule1[StringNode] = rule {
    "\"" ~ simpleStringChar.* ~ "\"" ~> ((s: Any) => StringNode(s.asInstanceOf[Seq[String]].foldLeft("")((a, v) => a + v)))
  }

  def TripleString: Rule1[StringNode] = rule {
    ("```" ~ tripleStringChar.* ~ "```") ~> ((s: Any) => StringNode(s.asInstanceOf[Seq[String]].foldLeft("")((a, v) => a + v)))
  }

  def Number = rule {
    DoubleNumber | IntNumber
  }

  def IntNumber = rule {
    capture(CharPredicate.Digit.+) ~> ((a: String) => IntNumberNode(a.toInt))
  }

  def DoubleNumber = rule {
    (capture(CharPredicate.Digit.+) ~ "." ~ capture(CharPredicate.Digit.+)) ~> ((a: String, b: String) => DoubleNumberNode(s"$a.$b".toDouble))
  }

  def Array: Rule1[ArrayNode] = rule {
    ("[" ~ wsnl ~ ws.? ~ (ObjectMemberValue * ("," ~ wsnl ~ ws.?)) ~ wsnl ~ ws.? ~ "]") ~> ((a: Any) => ArrayNode(a.asInstanceOf[Vector[AstNode]]))
  }

  def Ident = rule {
    capture(oneOrMore(IdentCharSet))
  }

  def ws = rule { oneOrMore(" ") }
  def nl = rule { oneOrMore("\n") }
  def wsnl = rule { ws.? ~ nl.? }

  def tripleStringChar: Rule1[String] = rule { tripleEscapedChar | capture(TripleCharSet) }
  def simpleStringChar: Rule1[String] = rule { simpleEscapedChar | capture(StringCharSet) }

  def tripleReservedChar: Rule1[String] = rule { capture("\\" | "'") }
  def simpleReservedChar: Rule1[String] = rule { capture("\\" | "\"") }

  def tripleEscapedChar: Rule1[String] = rule { """\""" ~ (capture(StringCharSet) | simpleReservedChar) }
  def simpleEscapedChar: Rule1[String] = rule { """\""" ~ (capture(StringCharSet) | simpleReservedChar) }

  lazy val IdentCharSet = InnerChar -- '{' -- '}' -- ':'
  lazy val StringCharSet = InnerChar -- "\"" ++ ' '
  lazy val TripleCharSet = InnerChar -- "`" ++ ' '
  lazy val InnerChar =  CharPredicate.Visible.++(CharPredicate('\u0400' to '\u04FF'))

}

package it.vigtig.lambda.syntax

import scala.util.parsing.combinator.{PackratParsers, Parsers}
import scala.util.parsing.input.{Position, Reader}

sealed trait Fragment {

  def pretty: String = this match {
    case Named(lhs, rhs)             => s"${lhs.pretty} = ${rhs.pretty}"
    case Bool(b)                     => b.toString
    case Identifier(id)              => id
    case Integer(i)                  => i.toString
    case Application(left, right)    => s"(${left.pretty}) ${right.pretty}"
    case LambdaAbstraction(id, body) => s"${id.pretty} . ${body.pretty}"
    case Nil                         => ""
  }

}

sealed trait Declaration extends Fragment
sealed trait Term        extends Fragment

case class Named(lhs: Identifier, rhs: Term) extends Declaration

case class Bool(b: Boolean)                        extends Term
case class Identifier(id: String)                  extends Term
case class Floating(f: Float)                      extends Term
case class Integer(i: Int)                         extends Term
case class Application(left: Term, right: Term)    extends Term
case class LambdaAbstraction(id: Term, body: Term) extends Term

case object Nil extends Term

object ParserExample extends App {

  val programString =
    """|x = 20
      |* ((a . b . + a b) x 1) 2""".stripMargin

  val ast = FraseLexer(programString).flatMap(Parser.apply)

  println(ast.map(_.mkString("\n")))
  println(ast.map(_.map(_.pretty).mkString("\n")))

}

class Parser extends Parsers with PackratParsers {

  override type Elem = Token

  def apply(tokens: List[Token]): Either[String, List[Fragment]] = {
    val reader = new PackratReader(TokenReader(tokens))
    program(reader) match {
      case NoSuccess(msg, _)  => Left(msg)
      case Success(result, _) => Right(result)
    }
  }

  lazy val fragment: PackratParser[Fragment] = (named | term) <~ NEWLINE.*

  lazy val program: PackratParser[List[Fragment]] = phrase(rep1(fragment))

  case class MyPos(line: Int, column: Int, lineContents: String) extends Position
  case class TokenReader(tokens: List[Token], position: Int = 1) extends Reader[Token] {
    override def first: Token        = tokens.head
    override def rest: Reader[Token] = TokenReader(tokens.drop(1), position + 1)
    override def pos: Position       = MyPos(1, position, tokens.mkString)
    override def atEnd: Boolean      = tokens.isEmpty
  }

  lazy val application: PackratParser[Term] = term ~ (pterm | terminal) ^^ {
    case left ~ right => Application(left, right)
  }

  private lazy val lambdaAbstraction = (identifier <~ (`.`)) ~ term ^^ {
    case id ~ body => LambdaAbstraction(id, body)
  }

  lazy val term: PackratParser[Term] = lambdaAbstraction | application | pterm | terminal

  lazy val terminal: PackratParser[Term] = integer | identifier

  lazy val pterm: PackratParser[Term] = `(` ~> term <~ `)`

  lazy val integer: PackratParser[Integer] =
    accept("integer literal", {
      case INTGR(str) => Integer(str.toInt)
    })

  lazy val identifier: PackratParser[Identifier] =
    accept("identifier", {
      case ID(str) => Identifier(str)
    })

  lazy val named: PackratParser[Named] = identifier ~ (`=` ~> term) ^^ {
    case id ~ term => Named(id, term)
  }

}

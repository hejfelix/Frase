package it.vigtig.lambda.syntax

import it.vigtig.lambda.errors.{FraseError, GenericError}
import it.vigtig.lambda.syntax.AST._

import scala.util.parsing.combinator.{PackratParsers, Parsers}
import scala.util.parsing.input.{Position, Reader}

object AST {
  sealed trait Fragment {

    def pretty: String = this match {
      case Named(lhs, rhs)             => s"${lhs.pretty} = ${rhs.pretty}"
      case Bool(b)                     => b.toString
      case Identifier(id)              => id
      case Integer(i)                  => i.toString
      case Application(left, right)    => s"(${left.pretty} ${right.pretty})"
      case LambdaAbstraction(id, body) => s"${id.pretty} . (${body.pretty})"
      case Empty                       => ""
    }

    def contains(term: Term): Boolean = {
      this match {
        case `term`                      => true
        case Named(lhs, rhs)             => lhs.contains(term) || rhs.contains(term)
        case Application(lhs, rhs)       => lhs.contains(term) || rhs.contains(term)
        case LambdaAbstraction(id, body) => id.contains(term) || body.contains(term)
        case _                           => false
      }
    }

  }

  sealed trait Declaration extends Fragment {}
  sealed trait Term extends Fragment {

    def transform(f: PartialFunction[Term, Term]): Term = {

      val fallback: PartialFunction[Term, Term] = {
        case Application(left, right) => Application(left.transform(f), right.transform(f))
        case LambdaAbstraction(id, body) => LambdaAbstraction(id.transform(f), body.transform(f))
        case x => x
      }
      (f orElse fallback) (this)
    }

    def relabel(oldLabel: Identifier, newLabel: Identifier): Term =
      transform {
        case `oldLabel` => newLabel
      }

  }

  case class Named(lhs: Identifier, rhs: Term) extends Declaration

  case class Bool(b: Boolean)                        extends Term
  case class Identifier(id: String)                  extends Term
  case class Floating(f: Float)                      extends Term
  case class Integer(i: Int)                         extends Term
  case class Application(left: Term, right: Term)    extends Term
  case class LambdaAbstraction(id: Term, body: Term) extends Term

  case object Empty extends Term
}

object ParserExample extends App {

  val programString =
    """|x = 20
      |* ((a . b . + a b) x 1) 2""".stripMargin

  val ast = DefaultParser(DefaultLexer()).parse(programString)

  println(ast.map(_.mkString("\n")))
  println(ast.map(_.map(_.pretty).mkString("\n")))

}

trait Parser {
  def parse(program: String): Either[FraseError, List[Fragment]]
  def parseFragment(fragment: String): Either[FraseError, Fragment]
}

case class DefaultParser(lexer: Lexer) extends Parser with Parsers with PackratParsers {

  override type Elem = Token

  override def parse(program: String): Either[FraseError, List[Fragment]] =
    lexer.tokenize(program).flatMap(parse)

  override def parseFragment(fragment: String): Either[FraseError, Fragment] =
    lexer
      .tokenize(fragment)
      .map(_.takeWhile(_ != NEWLINE))
      .flatMap(parseSingleFragment)

  private def parseSingleFragment(tokens: List[Token]): Either[FraseError, Fragment] = {
    val reader = new PackratReader(TokenReader(tokens))
    fragment(reader) match {
      case NoSuccess(msg, _)  => Left(GenericError(msg))
      case Success(result, _) => Right(result)
    }
  }

  private def parse(tokens: List[Token]): Either[FraseError, List[Fragment]] = {
    val reader = new PackratReader(TokenReader(tokens))
    program(reader) match {
      case NoSuccess(msg, _)  => Left(GenericError(msg))
      case Success(result, _) => Right(result)
    }
  }

  private lazy val fragment: PackratParser[Fragment] = (named | term) <~ NEWLINE.*

  private lazy val program: PackratParser[List[Fragment]] = phrase(rep1(fragment))

  private case class MyPos(line: Int, column: Int, lineContents: String) extends Position
  private case class TokenReader(tokens: List[Token], position: Int = 1) extends Reader[Token] {
    override def first: Token        = tokens.head
    override def rest: Reader[Token] = TokenReader(tokens.drop(1), position + 1)
    override def pos: Position       = MyPos(1, position, tokens.mkString)
    override def atEnd: Boolean      = tokens.isEmpty
  }

  private lazy val application: PackratParser[Term] = term ~ (pterm | terminal) ^^ {
    case left ~ right => Application(left, right)
  }

  private lazy val lambdaAbstraction = ((identifier | terminal) <~ (`.`)) ~ term ^^ {
    case id ~ body => LambdaAbstraction(id, body)
  }

  private lazy val term: PackratParser[Term] = lambdaAbstraction | application | pterm | terminal

  private lazy val terminal: PackratParser[Term] = integer | bool | identifier

  private lazy val pterm: PackratParser[Term] = `(` ~> term <~ `)`

  private lazy val bool: PackratParser[Bool] = accept("boolean literal", {
    case TRUE => Bool(true)
    case FALSE => Bool(false)
})

  private lazy val integer: PackratParser[Integer] =
    accept("integer literal", {
      case INTGR(str) => Integer(str.toInt)
    })

  private lazy val identifier: PackratParser[Identifier] =
    accept("identifier", {
      case ID(str) => Identifier(str)
    })

  private lazy val named: PackratParser[Named] = identifier ~ (`=` ~> term) ^^ {
    case id ~ term => Named(id, term)
  }

}

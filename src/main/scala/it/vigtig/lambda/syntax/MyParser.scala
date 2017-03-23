package it.vigtig.lambda.syntax

import scala.util.parsing.combinator.{PackratParsers, Parsers}
import scala.util.parsing.input.{NoPosition, Position, Reader}

sealed trait Fragment

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

case class BinOp(name: String, left: Term, right: Term) extends Term

object MyParserExample extends App {

  val programString =
    """|x = 20
      |* ((a . b . + a b) x 1) 2
    """.stripMargin

  FraseLexer(programString) match {
    case Right(tokens) =>
      println(tokens)
      println(MyParser(tokens))
  }

}

object MyParser extends Parsers with PackratParsers {

  override type Elem = Token

  def apply(tokens: List[Token]) = {
    val reader = new PackratReader(TokenReader(tokens))
    program(reader) match {
      case NoSuccess(msg, _)  => Left(msg)
      case Success(result, _) => Right(result)
    }
  }


  lazy val plus: PackratParser[Term] = (PLUS ~> term) ~ term ^^ {
    case arg1 ~ arg2 => BinOp("+", arg1, arg2)
  }
  lazy val multiply: PackratParser[Term] = (TIMES ~> term) ~ term ^^ {
    case arg1 ~ arg2 => BinOp("*", arg1, arg2)
  }

//  lazy val multiply: PackratParser[Term] = `*` ~> term ~ term ^^ {
//    case arg1 ~ arg2 => BinOp("*", arg1, arg2)
//  }

  lazy val fragment: PackratParser[Fragment] = (named | term) <~ NEWLINE

  lazy val program: PackratParser[List[Fragment]] = phrase(rep1(fragment))

  case class MyPos(line: Int, column: Int, lineContents: String) extends Position
  case class TokenReader(tokens: List[Token], position: Int = 1) extends Reader[Token] {
    override def first: Token        = tokens.head
    override def rest: Reader[Token] = TokenReader(tokens.drop(1), position + 1)
    override def pos: Position       = MyPos(1, position, tokens.mkString)
    override def atEnd: Boolean      = tokens.isEmpty
  }

  lazy val application: PackratParser[Term] = term ~ term ^^ {
    case left ~ right => Application(left, right)
  }

  private lazy val pureLambdaAbstraction = (term <~ `.`) ~ term ^^ {
    case id ~ body => LambdaAbstraction(id, body)
  }
  lazy val lambdaAbstraction: PackratParser[Term] = pureLambdaAbstraction | plus | multiply

  lazy val term: PackratParser[Term] = lambdaAbstraction | terminal | application |  pterm

  lazy val terminal: PackratParser[Term] = integer | identifier

  lazy val pterm: PackratParser[Term] = `(` ~> term <~ `)`

  lazy val integer: PackratParser[Integer] =
    accept("integer literal", {
      case FINTEGER(str) => Integer(str.toInt)
    })

  lazy val identifier: PackratParser[Identifier] =
    accept("identifier", {
      case FIDENTIFIER(str) => Identifier(str)
    })

  lazy val named: PackratParser[Named] = identifier ~ (`=` ~> term) ^^ {
    case id ~ term => Named(id, term)
  }
//  lazy val named: PackratParser[Named] =

}

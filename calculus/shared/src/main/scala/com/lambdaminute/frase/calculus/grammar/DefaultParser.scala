package com.lambdaminute.frase.calculus.grammar

import com.lambdaminute.frase.calculus.ast.Ast._
import com.lambdaminute.frase.calculus.errors.{FraseError, _}

import scala.util.parsing.combinator.{PackratParsers, Parsers}
import scala.util.parsing.input.{Position, Reader}

case class DefaultParser(lexer: Lexer) extends Parser with Parsers with PackratParsers {

  override type Elem = Token

  override def parse(program: String): Either[FraseError, Term] =
    lexer.tokenize(program).flatMap(parse)

  override def parseFragment(fragment: String): Either[FraseError, Term] =
    lexer
      .tokenize(fragment)
      .map(_.takeWhile(_ != NEWLINE))
      .flatMap(parseSingleFragment)

  override def parseTerm(term: String): Either[FraseError, Term] =
    lexer.tokenize(term).flatMap(parseSingleTerm)

  private def parseSingleTerm(tokens: List[Token]) = {
    val reader = new PackratReader(TokenReader(tokens))
    term(reader) match {
      case NoSuccess(msg, next) => Left(ParsingError(msg, next))
      case Success(result, _)   => Right(result)
    }
  }

  private def parseSingleFragment(tokens: List[Token]): Either[FraseError, Term] = {
    val reader = new PackratReader(TokenReader(tokens))
    term(reader) match {
      case NoSuccess(msg, next) => Left(ParsingError(msg, next))
      case Success(result, _)   => Right(result)
    }
  }

  private def parse(tokens: List[Token]): Either[FraseError, Term] = {
    val reader: PackratReader[Token] = new PackratReader(TokenReader(tokens))
    program(reader) match {
      case NoSuccess(msg, next) => Left(ParsingError(msg, next))
      case Success(result, _)   => Right(result)
    }
  }

  private lazy val program: PackratParser[Term] = phrase(term)

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

  private lazy val lambdaAbstraction = ((identifier | terminal) <~ `.`) ~ term ^^ {
    case id ~ body => LambdaAbstraction(id, body)
  }

  private lazy val term: PackratParser[Term] = lambdaAbstraction | application | pterm | terminal

  private lazy val terminal: PackratParser[Term] = float | integer | bool | identifier

  private lazy val pterm: PackratParser[Term] = `(` ~> term <~ `)`

  private lazy val bool: PackratParser[Bool] = accept("boolean literal", {
    case TRUE  => Bool(true)
    case FALSE => Bool(false)
  })

  private lazy val integer: PackratParser[Integer] =
    accept("integer literal", {
      case INTGR(str) => Integer(str.toInt)
    })

  private lazy val float: PackratParser[Floating] =
    accept("floating point literal", {
      case FLOAT(str) => Floating(str.toFloat)
    })

  private lazy val identifier: PackratParser[Identifier] =
    accept("identifier", {
      case ID(str) => Identifier(str)
    })

}

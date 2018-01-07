package com.lambdaminute.syntax

import com.lambdaminute.errors.{FraseError, GenericError}
import com.lambdaminute.syntax.AST._

import scala.util.parsing.combinator.{PackratParsers, Parsers}
import scala.util.parsing.input.{Position, Reader}



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
    case TRUE  => Bool(true)
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

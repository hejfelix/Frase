package com.lambdaminute.frase.syntax

import com.lambdaminute.frase.calculus.ast.Ast.Term
import com.lambdaminute.frase.calculus.errors.{FraseError, LexerError}
import com.lambdaminute.frase.calculus.grammar.{DefaultLexer, DefaultParser, Token}
import com.lambdaminute.frase.calculus.semantic.DefaultKeywords

object DefaultParserInstances {
  def defaultKeywords = DefaultKeywords()
  def lexer           = DefaultLexer()
  def parser          = DefaultParser(lexer)
}

trait ParserSyntax {

  implicit class ParserSyntax(s: String) {
    import DefaultParserInstances._
    def parseTerm: Either[FraseError, Term]                      = parser.parseTerm(s)
    def parse: Either[FraseError, Term]                          = parser.parse(s)
    def lex: Either[LexerError[DefaultLexer#Input], List[Token]] = lexer.tokenize(s: String)

    def parseTermUnsafe: Term  = parser.parseTerm(s).right.get
    def parseUnsafe: Term      = parser.parse(s).right.get
    def lexUnsafe: List[Token] = lexer.tokenize(s: String).right.get
  }

}

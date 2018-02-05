package com.lambdaminute.syntax

import com.lambdaminute.ast.AST
import com.lambdaminute.errors.{FraseError, LexerError}
import com.lambdaminute.grammar.{DefaultLexer, DefaultParser, Token}
import com.lambdaminute.semantic.DefaultKeywords

object DefaultParserInstances {
  def defaultKeywords = DefaultKeywords()
  def lexer           = DefaultLexer()
  def parser          = DefaultParser(lexer)
}

trait ParserSyntax {

  implicit class ParserSyntax(s: String) {
    import DefaultParserInstances._
    def parseTerm: Either[FraseError, AST.Term]                  = parser.parseTerm(s)
    def parse: Either[FraseError, List[AST.Fragment]]            = parser.parse(s)
    def lex: Either[LexerError[DefaultLexer#Input], List[Token]] = lexer.tokenize(s: String)

    def parseTermUnsafe: AST.Term       = parser.parseTerm(s).right.get
    def parseUnsafe: List[AST.Fragment] = parser.parse(s).right.get
    def lexUnsafe: List[Token]          = lexer.tokenize(s: String).right.get
  }

}

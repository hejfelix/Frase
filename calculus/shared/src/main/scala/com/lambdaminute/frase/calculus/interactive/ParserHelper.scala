package com.lambdaminute.frase.calculus.interactive

import com.lambdaminute.frase.calculus.ast.Ast.Term
import com.lambdaminute.frase.calculus.grammar.{DefaultLexer, DefaultParser}

trait ParserHelper {

  private val lexer  = DefaultLexer()
  private val parser = DefaultParser(lexer)
  implicit class StringParsable(s: String) {
    def toTerm: Term = {
      val parsedResult = parser.parseTerm(s)
      if (parsedResult.isLeft) {
        sys.error(parsedResult.toString)
      }
      parsedResult.right.get
    }
  }

}

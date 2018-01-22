package com.lambdaminute.interactive

import com.lambdaminute.syntax.AST.Term
import com.lambdaminute.syntax.{DefaultLexer, DefaultParser}
import com.lambdaminute.types.Type
import com.lambdaminute.types._

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
    def toType: Type = parser.parseTerm(s).right.get.asType
  }
  implicit def tupleToTypeJudgement(strs: (String, String)): (Term, Type) = strs._1.toTerm -> strs._2.toType

}

package com.lambdaminute.syntax

import com.lambdaminute.ast.AST
import com.lambdaminute.errors.FraseError
import com.lambdaminute.interpreter.{DefaultInterpreter, DefaultLetTransformer, Interpreter}

trait InterpreterSyntax {
  import DefaultParserInstances._

  private def letTransformer           = DefaultLetTransformer(defaultKeywords)
  private def interpreter: Interpreter = DefaultInterpreter(parser, letTransformer, defaultKeywords)

  implicit class InterpreterSyntax(s: String) {
    def interpret: Either[FraseError, AST.Fragment] = interpreter.interpret(s)
    def interpretUnsafe: AST.Fragment               = interpreter.interpret(s).right.get
  }

}

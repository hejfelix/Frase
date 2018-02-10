package com.lambdaminute.frase.syntax

import com.lambdaminute.frase.calculus.ast.AST
import com.lambdaminute.frase.calculus.errors.FraseError
import com.lambdaminute.frase.calculus.interpreter.{DefaultBuiltins, DefaultInterpreter, DefaultLetTransformer, Interpreter}

trait InterpreterSyntax {
  import DefaultParserInstances._

  private def letTransformer = DefaultLetTransformer(defaultKeywords)
  private def interpreter: Interpreter =
    DefaultInterpreter(parser, letTransformer, defaultKeywords, DefaultBuiltins.builtIns)

  implicit class InterpreterSyntax(s: String) {
    def interpret: Either[FraseError, AST.Fragment] = interpreter.interpret(s)
    def interpretUnsafe: AST.Fragment               = interpreter.interpret(s).right.get
  }

}

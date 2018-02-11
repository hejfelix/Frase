package com.lambdaminute.frase.syntax

import com.lambdaminute.frase.calculus.ast.AST
import com.lambdaminute.frase.calculus.errors.FraseError
import com.lambdaminute.frase.calculus.interpreter.{DefaultBuiltins, DefaultInterpreter, Interpreter}

trait InterpreterSyntax {
  import DefaultParserInstances._

  private def interpreter: Interpreter =
    DefaultInterpreter(parser, defaultKeywords, DefaultBuiltins.builtIns)

  implicit class InterpreterSyntax(s: String) {
    def interpret: Either[FraseError, AST.Term] = interpreter.interpret(s)
    def interpretUnsafe: AST.Term               = interpreter.interpret(s).right.get
  }

}

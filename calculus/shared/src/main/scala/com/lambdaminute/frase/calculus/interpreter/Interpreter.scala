package com.lambdaminute.frase.calculus.interpreter

import com.lambdaminute.frase.calculus.ast.AST.Term
import com.lambdaminute.frase.calculus.errors.FraseError

trait Interpreter {
  def interpret(program: String): Either[FraseError, Term]
  def interpret(term: Term): Either[FraseError, Term]
}

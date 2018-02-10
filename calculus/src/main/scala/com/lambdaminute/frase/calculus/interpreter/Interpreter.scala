package com.lambdaminute.frase.calculus.interpreter

import com.lambdaminute.frase.calculus.errors.FraseError
import com.lambdaminute.frase.calculus.ast.AST.{Fragment, Term}

trait Interpreter {
  def interpret(program: String): Either[FraseError, Fragment]
  def interpret(fragment: Fragment, namedTerms: List[Fragment]): Either[FraseError, Term]



}

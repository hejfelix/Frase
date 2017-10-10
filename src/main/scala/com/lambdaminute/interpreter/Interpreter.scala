package com.lambdaminute.interpreter

import com.lambdaminute.errors.FraseError
import com.lambdaminute.syntax.AST.{Fragment, Term}

trait Interpreter {
  def interpret(program: String): Either[FraseError, Fragment]
  def interpret(fragment: Fragment, namedTerms: List[Fragment]): Either[FraseError, Term]
}

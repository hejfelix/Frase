package com.lambdaminute.frase.calculus.grammar

import com.lambdaminute.frase.calculus.errors.FraseError
import com.lambdaminute.frase.calculus.ast.AST.{Fragment, Term}

trait Parser {
  def parse(program: String): Either[FraseError, List[Fragment]]
  def parseFragment(fragment: String): Either[FraseError, Fragment]
  def parseTerm(term: String): Either[FraseError, Term]
}

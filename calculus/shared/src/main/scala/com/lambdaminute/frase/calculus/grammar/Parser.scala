package com.lambdaminute.frase.calculus.grammar

import com.lambdaminute.frase.calculus.errors.FraseError
import com.lambdaminute.frase.calculus.ast.Ast.Term

trait Parser {
  def parse(program: String): Either[FraseError, Term]
  def parseFragment(fragment: String): Either[FraseError, Term]
  def parseTerm(term: String): Either[FraseError, Term]
}

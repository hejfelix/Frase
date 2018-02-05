package com.lambdaminute.grammar

import com.lambdaminute.errors.FraseError
import com.lambdaminute.ast.AST.{Fragment, Term}

trait Parser {
  def parse(program: String): Either[FraseError, List[Fragment]]
  def parseFragment(fragment: String): Either[FraseError, Fragment]
  def parseTerm(term: String): Either[FraseError, Term]
}

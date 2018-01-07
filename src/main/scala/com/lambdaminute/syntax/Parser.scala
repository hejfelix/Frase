package com.lambdaminute.syntax

import com.lambdaminute.errors.FraseError
import com.lambdaminute.syntax.AST.{Fragment, Term}

trait Parser {
  def parse(program: String): Either[FraseError, List[Fragment]]
  def parseFragment(fragment: String): Either[FraseError, Fragment]
  def parseTerm(term: String): Either[FraseError, Term]
}

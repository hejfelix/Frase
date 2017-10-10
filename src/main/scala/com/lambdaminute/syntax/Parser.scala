package com.lambdaminute.syntax

import com.lambdaminute.errors.FraseError
import com.lambdaminute.syntax.AST.Fragment

trait Parser {
  def parse(program: String): Either[FraseError, List[Fragment]]
  def parseFragment(fragment: String): Either[FraseError, Fragment]
}

package com.lambdaminute.interpreter

import com.lambdaminute.errors.FraseError
import com.lambdaminute.syntax.AST._

trait LetTransformer {

  def transform(fragments: List[Fragment]): Either[FraseError, Term]

}



package com.lambdaminute.interpreter

import com.lambdaminute.errors.FraseError
import com.lambdaminute.ast.AST._

trait LetTransformer {

  def transform(fragments: List[Fragment]): Either[FraseError, Term]

}



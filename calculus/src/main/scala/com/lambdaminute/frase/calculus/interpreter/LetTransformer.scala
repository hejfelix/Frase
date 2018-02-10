package com.lambdaminute.frase.calculus.interpreter

import com.lambdaminute.frase.calculus.errors.FraseError
import com.lambdaminute.frase.calculus.ast.AST._

trait LetTransformer {

  def transform(fragments: List[Fragment]): Either[FraseError, Term]

}



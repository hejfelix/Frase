package com.lambdaminute.frase.lang

import com.lambdaminute.frase.calculus.ast.Ast._
import com.lambdaminute.frase.calculus.errors.FraseError

trait LetTransformer {

  def transform(fragments: List[Term]): Either[FraseError, Term]

}

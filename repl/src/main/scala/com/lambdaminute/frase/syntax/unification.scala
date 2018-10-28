package com.lambdaminute.frase.syntax

import com.lambdaminute.frase.calculus.ast.Ast.Term
import com.lambdaminute.frase.calculus.errors.FraseError
import com.lambdaminute.frase.lang.types.DefaultUnification

trait UnificationSyntax {
  private val unification = new DefaultUnification()
  implicit class UnificationSyntax(t: Term) {
    def unify(that: Term): Either[FraseError, List[(Term, Term)]] = unification.unify(List(t -> that))
    def unifyUnsafe(that: Term): List[(Term, Term)]               = unify(that).right.get
  }
}

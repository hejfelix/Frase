package com.lambdaminute.syntax

import com.lambdaminute.ast.AST.Term
import com.lambdaminute.types.DefaultUnification

trait UnificationSyntax {
  private val unification = new DefaultUnification()
  implicit class UnificationSyntax(t: Term) {
    def unify(that: Term): Either[String, List[(Term, Term)]] = unification.unify(List(t -> that))
    def unifyUnsafe(that: Term): List[(Term, Term)]           = unify(that).right.get
  }
}

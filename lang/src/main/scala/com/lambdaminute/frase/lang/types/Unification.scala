package com.lambdaminute.frase.lang.types
import com.lambdaminute.frase.calculus.ast.Ast.Term
import com.lambdaminute.frase.calculus.errors.FraseError

trait Unification {
  object syntax {
    implicit class UnifyTerm(t: Term) {
      def unify(that: Term): Either[FraseError, List[(Term, Term)]] = unifyFix(List(t -> that))
    }
  }
  def unifyFix(eqs: List[(Term, Term)]): Either[FraseError, List[(Term, Term)]]
}

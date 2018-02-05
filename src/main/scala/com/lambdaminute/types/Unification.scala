package com.lambdaminute.types
import com.lambdaminute.ast.AST.Term

trait Unification {
  object syntax {
    implicit class UnifyTerm(t: Term) {
      def unify(that: Term): Either[String, List[(Term, Term)]] = unifyFix(List(t -> that))
    }
  }
  def unifyFix(eqs: List[(Term, Term)]): Either[String, List[(Term, Term)]]
}

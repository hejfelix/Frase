package com.lambdaminute.types
import com.lambdaminute.syntax.AST.Term

trait Unification {
  def unifyFix(eqs: List[(Term, Term)]): Either[String, List[(Term, Term)]]
}

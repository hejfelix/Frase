package it.vigtig.lambda

trait Unification extends AST {
  def unify(a: Term, b: Term): Map[Id, Term]
}

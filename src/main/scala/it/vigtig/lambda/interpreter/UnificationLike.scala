package it.vigtig.lambda.interpreter

import it.vigtig.lambda.AST
import it.vigtig.lambda.syntax.AST._

trait UnificationLike {

  def maybeUnion[A, B](a: Option[Map[A, B]], b: Option[Map[A, B]]): Option[Map[A, B]] =
    for (x <- a; y <- b) yield x ++ y

  def unify(a: Term, b: Term): Option[Map[Term, Term]] = (a, b) match {
    case (x: Identifier, y: Identifier) if x == y           => Some(Map())
    case (x: Identifier, y)                                 => Some(Map(x -> y))
    case (x, y: Identifier)                                 => Some(Map(y -> x))
    case (Application(a, b), Application(x, y))             => maybeUnion(unify(a, x), unify(b, y))
    case (LambdaAbstraction(a, b), LambdaAbstraction(x, y)) => maybeUnion(unify(a, x), unify(b, y))
    case (x, y) if x == y                                   => Some(Map())
    case _                                                  => None
  }

  def unifyLists(xs: List[Term], ys: List[Term]) =
    if (xs.size != ys.size)
      None
    else
      (xs, ys).zipped.map(unify).reduce(maybeUnion[Term, Term])

  def header(n: Term): List[Term] = n match {
//    case Named(id, body) => header(body)
//    case Application(s @ SetId(_), body)   => s :: header(body)
    case LambdaAbstraction(x, body @ LambdaAbstraction(_, _)) => x :: header(body)
    case LambdaAbstraction(x, body)                           => List(x)
    case term                                                 => Nil
  }

  def stripHeader(n: Term): Term = n match {
    case LambdaAbstraction(_, body) => stripHeader(body)
    case _                          => n
  }

}

abstract trait Unification extends AST {
  def unify(a: Term, b: Term): Map[Id, Term]
}

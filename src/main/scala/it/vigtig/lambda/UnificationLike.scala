package it.vigtig.lambda

/**
 * @author Felix
 */
trait UnificationLike extends ASTLike {

  def maybeUnion[A, B](a: Option[Map[A, B]], b: Option[Map[A, B]]) =
    for (x <- a; y <- b) yield x ++ y

  def unify(a: Term, b: Term): Option[Map[Id, Term]] = (a, b) match {
    case (x: Id, y: Id) if x == y     => Some(Map())
    case (x: Id, y)                   => Some(Map(x -> y))
    case (x, y: Id)                   => Some(Map(y -> x))
    case (Applic(a, b), Applic(x, y)) => maybeUnion(unify(a, x), unify(b, y))
    case (Abstr(a, b), Abstr(x, y))   => maybeUnion(unify(a, x), unify(b, y))
    case (x, y) if (x == y)           => Some(Map())
    case _                            => None
  }

}

abstract trait Unification extends AST {
  def unify(a: Term, b: Term): Map[Id, Term]
}

object UnificationTest extends UnificationLike with App {

  val x = Applic(Applic(Applic(Applic(Id("Cons"), Integer(42)), Id("Cons")), Integer(1337)), Id("Nil"))
  val y = Applic(Applic(Applic(Applic(Id("Cons"), Id("x")), Id("Cons")), Id("y")), Id("Nil"))

  println(unify(x, y))

}
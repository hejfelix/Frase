package it.vigtig.lambda

/**
 * @author Felix
 */
trait UnificationLike extends ASTLike {

  def maybeUnion[A, B](a: Option[Map[A, B]], b: Option[Map[A, B]]) =
    for (x <- a; y <- b) yield x ++ y

  def unify(a: Term, b: Term): Option[Map[Id, Term]] = (a, b) match {
    case (x: Id, y)                   => Some(Map(x -> y))
    case (x, y: Id)                   => Some(Map(y -> x))
    case (Applic(a, b), Applic(x, y)) => maybeUnion(unify(a, x), unify(b, y))
    case (Abstr(a, b), Abstr(x, y))   => maybeUnion(unify(a, x), unify(b, y))
    case _                            => None
  }

}

abstract trait Unification extends AST {
  def unify(a: Term, b: Term): Map[Id, Term]
}

object UnificationTest extends UnificationLike with App {

  val x = Applic(Id("myId"), Applic(Id("y"), Id("k")))
  val y = Applic(Abstr(Id("x"), Id("x")), Id("l"))

  println(unify(x, y).mkString("\n"))

}
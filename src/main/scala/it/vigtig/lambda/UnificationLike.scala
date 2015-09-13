package it.vigtig.lambda

/**
 * @author Felix
 */
trait UnificationLike extends ASTLike {

  def maybeUnion[A, B](a: Option[Map[A, B]], b: Option[Map[A, B]]): Option[Map[A, B]] =
    for (x <- a; y <- b) yield x ++ y

  def unify(a: Term, b: Term): Option[Map[Term, Term]] = (a, b) match {
    case (x: Id, y: Id) if x == y     => Some(Map())
    case (x: Id, y)                   => Some(Map(x -> y))
    case (x, y: Id)                   => Some(Map(y -> x))
    case (Abstr(a, b), Applic(x, y))  => maybeUnion(unify(a, Applic(x, y)), unify(b, y))
    case (Applic(a, b), Applic(x, y)) => maybeUnion(unify(a, x), unify(b, y))
    case (Abstr(a, b), Abstr(x, y))   => maybeUnion(unify(a, x), unify(b, y))
    case (Abstr(a, b), x)             => unify(a, x)
    case (x, y) if x == y             => Some(Map())
    case _                            => None
  }

  def unifyLists(xs: List[Term], ys: List[Term]) =
    if (xs.size != ys.size)
      None
    else
      (xs, ys).zipped.map(unify).reduce(maybeUnion[Term, Term])

  def header(n: Term): List[Term] = n match {
    case Named(id, body)            => header(body)
    case Applic(s@SetId(_), body)   => s :: header(body)
    case Abstr(x, body@Abstr(_, _)) => x :: header(body)
    case Abstr(x, body)             => List(x)
    case term                       => Nil
  }

  def stripHeader(n: Term): Term = n match {
    case Abstr(_, body) => stripHeader(body)
    case _              => n
  }

}

abstract trait Unification extends AST {
  def unify(a: Term, b: Term): Map[Id, Term]
}

object UnificationTest extends UnificationLike with App with ParserLike {

  val x = parseAll(LINE, "Cons 42 (Cons 1337 Nil)")
  val y = parseAll(LINE, "Cons x xs")

  val func = parseAll(LINE, "head = (Cons x xs) . x")

  println("x:" + x.get)
  println("y:" + y.get)
  println()
  println(unify(x.get, y.get))


  println(func.get)

  println()
  println("header: " + header(func.get))
  println()
  println("unified header: " + unify(header(func.get).head, x.get))
}
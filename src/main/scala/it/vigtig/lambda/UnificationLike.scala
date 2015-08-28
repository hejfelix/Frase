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

  def header(n:Term):Term = n match {
    case Named(id,body) => header(body)
    case Abstr(x,body) => Abstr(x,header(body))
    case term => Empty
  }
  
}

abstract trait Unification extends AST {
  def unify(a: Term, b: Term): Map[Id, Term]
}

object UnificationTest extends UnificationLike with App with ParserLike {

  val x = parseAll(LINE,"Cons 42 (Cons 1337 Nil)")
  val y = parseAll(LINE,"Cons x xs")

  val func = parseAll(LINE,"func = Cons x . y . + x y")
  
  println("x:"+x.get)
  println("y:"+y.get)
  println(unify(x.get,y.get))
  
  
  println(func.get)

  println("header: "+header(func.get))
}
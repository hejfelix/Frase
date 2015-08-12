package it.vigtig.lambda

/**
 * @author Hargreaves
 */
object Interpreter extends Parser with App {
  import LambdaAST._

  val TEST1 = "( y . (x . y z x ) a) b"
  val TEST2 = "a b c d"
  val TEST3 = "a = + 1 2 true - .32f"

  val TESTNAMED =
    """
      a = ( y . (x . y z x ) a) b
      
      b = a x b
      """

  parseAll(PRGM, TEST3) match {
    case Success(lup, _) =>
      println(lup)
      println(lup.map(prettyStr).mkString)
    //      println("  beta-reduction --->")
    //      println( lup.map( (prettyStr _).compose(reduce)).mkString )
    case x => println(x)
  }

  def interpret(t: Term):Term = fixPoint(t)(reduce)

  def reduce(t: Term): Term = t match {
    case Id(_)                        => t
    case Named(id, body)              => Named(id, reduce(body))
    case Applic(Abstr(id, body), rhs) => reduce(betaReduce(rhs, id, body))
    case Applic(t, y)                 => Applic(reduce(t), reduce(y))
    case Abstr(a, b)                  => Abstr(a, reduce(b))
    case _                            => t
  }

  def prettyStr(t: Term): String = t match {
    case Applic(a, b)       => s"(${prettyStr(a)} ${prettyStr(b)})"
    case Abstr(Id(x), b)    => s"$x . ${prettyStr(b)}"
    case Id(x)              => x
    case Named(Id(x), term) => s"$x = ${prettyStr(term)}"
    case Empty              => "<Empty>"
    case Integer(i)         => i.toString
    case Floating(f)        => f.toString
    case Bit(b)             => b.toString
  }

  def fixPoint[T](t: T)(p: T => T): T =
    if (p(t) != t)
      fixPoint(p(t))(p)
    else
      t

  def betaReduce(newX: Term, oldX: Id, in: Term): Term = in match {
    case `oldX` => newX
    case Id(_)  => in
    case Abstr(id, body) =>
      Abstr(id, betaReduce(newX, oldX, body))
    case Applic(l, r) =>
      Applic(betaReduce(newX, oldX, l), betaReduce(newX, oldX, r))
  }

}
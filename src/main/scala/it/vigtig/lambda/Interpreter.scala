package it.vigtig.lambda

/**
 * @author Hargreaves
 */
object Interpreter extends Parser with App {
  import LambdaAST._

  val TEST1 = "( y . (x . y z x ) a) b"
  val TEST2 = "a b c d"
  val TESTNAMED = "a = ( y . (x . y z x ) a) b"

  parseAll(PRGM, TESTNAMED) match {
    case Success(lup, _) =>
      println(lup.map(prettyStr).mkString)
      println("  beta-reduction --->")
      println( lup.map( (prettyStr _).compose(reduce)).mkString )
    case x => println(x)
  }

  def interpret(term: Term) = betaReduction(term)

  def reduce(t: Term): Term = t match {
    case Id(_) => t
    case Named(id,body) => Named(id,reduce(body))
    case Application(Abstraction(id, body), rhs) =>
      reduce(betaReduce(rhs, id, body))
    case Application(t, y) => Application(reduce(t), reduce(y))
    case Abstraction(a, b) => Abstraction(a, reduce(b))
    case _                 => t
  }

  def prettyStr(t: Term): String = t match {
    case Application(a, b) =>
      s"(${prettyStr(a)} ${prettyStr(b)})"
    case Abstraction(Id(x), b) =>
      s"$x . ${prettyStr(b)}"
    case Id(x)              => x
    case Named(Id(x), term) => s"$x = ${prettyStr(term)}"
  }

  def fixPoint[T](t: T)(p: T => T): T =
    if (p(t) != t)
      fixPoint(p(t))(p)
    else
      t

  def betaReduction(t: Term): Term = fixPoint(t)(reduce)

  def betaReduce(newX: Term, oldX: Id, in: Term): Term = in match {
    case `oldX` => newX
    case Id(_)  => in
    case Abstraction(id, body) =>
      Abstraction(id, betaReduce(newX, oldX, body))
    case Application(l, r) =>
      Application(betaReduce(newX, oldX, l), betaReduce(newX, oldX, r))
  }

}
package it.vigtig.lambda

/**
 * @author Hargreaves
 */

trait InterpreterLike {
  import LambdaAST._
  def interpret(t: Term): Term = fixPoint(t)(betaReduce)



  def size(t: Term): Int = t match {
    case Id(_)                        => 1
    case Named(id, body)              => 1 + size(id) + size(body)
    case Applic(t, y)                 => 1 + size(t) + size(y)
    case Abstr(a, b)                  => 1 + size(a) + size(b)
    case _                            => 1
  }

  def builtIns:PartialFunction[Term,Term] = {
    case Applic(Applic(Id("=="),a),b) => if(a==b) Bit(true) else Bit(false)
    case Applic(Applic(Id("+"),Integer(x)),Integer(y)) => Integer(x+y)
    case Applic(Applic(Bit(p),yes),no) => if(p) yes else no
  }
  def betaReduce:PartialFunction[Term,Term] = builtIns orElse {
    case i@Id(_)                      => i
    case Named(id, body)              => Named(id, betaReduce(body))
    case Applic(Abstr(id, body), rhs) => betaReduce(substitute(body)(id -> rhs))
    case Applic(t, y)                 => Applic(betaReduce(t), betaReduce(y))
    case Abstr(a, b)                  => Abstr(a, betaReduce(b))
    case t                            => t
  }

  def prettyStr(t: Term): String = t match {
    case Applic(a @ Id(_), b) => s"${prettyStr(a)} ${prettyStr(b)}"
    case Applic(a, b)         => s"(${prettyStr(a)}) (${prettyStr(b)})"
    case Abstr(Id(x), b)      => s"$x . ${prettyStr(b)}"
    case Id(x)                => x
    case Named(Id(x), term)   => s"$x = ${prettyStr(term)}"
    case Empty                => "< >"
    case Integer(i)           => i.toString
    case Floating(f)          => f.toString
    case Bit(b)               => b.toString
  }

  def fixPoint[T](t: T)(p: T => T): T =
    if (p(t) != t)
      fixPoint(p(t))(p)
    else
      t

  def freeVars(t: Term): Set[Id] = t match {
    case a @ Id(_)       => Set(a)
    case Abstr(id, body) => freeVars(body) - id
    case Applic(a, b)    => freeVars(a) ++ freeVars(b)
    case Empty           => Set()
    case Named(id, term) => freeVars(term)
    case _         => Set()
  }

  //Capture-avoiding substitution
  def substitute(t: Term)(label: (Id, Term)): Term = (t, label) match {
    case (i: Id, (j, k)) if i == j => k
    case (i: Id, _)                => i
    case (Applic(a, b), _)         => Applic(substitute(a)(label), substitute(b)(label))
    case (Abstr(id, body), (x, y)) if id != x && !(freeVars(y)(id)) =>
      Abstr(id, substitute(body)(label))
    case (a @ Abstr(_, _), _) => a
  }

}

object Interpreter extends Parser with InterpreterLike with App {
  import LambdaAST._

  val TEST1 = "( y . (x . y z x ) a) b"
  val TEST2 = "a b c d"
  val TEST3 = "a = + 1 2 true - .32f"
  val TEST_AMB = "(x . x . x) (y 42)"

  val TESTNAMED =
    """
      true = x . y . x
      false = x . y . y
      b = a x b
      
      
      """

  val label = Id("y") -> Id("y")

  parseAll(PRGM, TESTNAMED) match {
    case Success(lup, _) =>
      val nameds = lup filter {
        case n: Named => true
        case _        => false
      }

      val dict = nameds.map {
        case Named(a, b) => a -> b
      }.toMap

      println("DICT:" + dict)
      println((nameds map prettyStr).mkString("\n"))
      println(s"free vars: ${(lup map freeVars).mkString}")
      println("  beta-reduction --->")
      println(lup.map((prettyStr _).compose(betaReduce)).mkString)
    case x => println(x)
  }

}
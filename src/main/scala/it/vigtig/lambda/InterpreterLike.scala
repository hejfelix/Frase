package it.vigtig.lambda

/**
 * @author Hargreaves
 */

trait InterpreterLike extends ParserLike {
  import AST._

  def interpretProgram(program: String): Option[List[Term]] = {

    parseAll(PRGM, program) match {

      case Success(lup, _) =>

        val (nameds, unnameds) = lup filter (_ != Empty) partition {
          case n: Named => true
          case _        => false
        }

        val dict: Map[Id, Term] = nameds.map {
          case Named(a, b) => a -> b
        }.toMap

        Some(unnameds.map(t => {
          val res = interpret(t)(dict)
          interpret(res)()
        }))

      case x => println(x); None
    }

  }

  def interpret(t: Term)(context: Map[Id, Term] = Map()): Term = fixPoint(t)(evalStep(context))

  def show[T](t: T): T = {
    println(t)
    t
  }

  def evalStep(context: Map[Id, Term])(t: Term) =
    fixPoint(resolve(t)(context))(reducer)

  def resolve(t: Term)(context: Map[Id, Term]): Term = context.foldLeft(t)((a, b) => substitute(a)(b))

  def size(t: Term): Int = t match {
    case Id(_)           => 1
    case Named(id, body) => 1 + size(id) + size(body)
    case Applic(t, y)    => 1 + size(t) + size(y)
    case Abstr(a, b)     => 1 + size(a) + size(b)
    case _               => 1
  }

  val App = Applic
  def builtIns: PartialFunction[Term, Term] = {
    case App(App(Id("=="), a), b)                    => Bit(a == b)
    case App(App(Id("*"), Integer(x)), Integer(y))   => Integer(x * y)
    case App(App(Id("+"), Integer(x)), Integer(y))   => Integer(x + y)
    case App(App(Id("+"), Floating(x)), Floating(y)) => Floating(x + y)
    case App(App(Id("-"), Integer(x)), Integer(y))   => Integer(x - y)
    case App(App(Bit(p), yes), no)                   => if (p) yes else no
    case App(App(Id("%"), Integer(a)), Integer(b))   => Integer(a % b)
    case App(App(Id("<="), Integer(a)), Integer(b))  => Bit(a <= b)
  }

  def reducer = {
    def betaReduce: PartialFunction[Term, Term] = builtIns orElse {
      case Named(id, body)              => Named(id, betaReduce(body))
      case Applic(Abstr(id, body), rhs) => betaReduce(substitute(body)(id -> rhs))
      case Applic(t, y)                 => Applic(betaReduce(t), betaReduce(y))
      case Abstr(a, b)                  => Abstr(a, betaReduce(b))
      case i @ Id(_)                    => i
      case t                            => t
    }
    betaReduce
  }

  def fixPoint[T](t: T)(p: T => T): T = {
    if (p(t) != t)
      fixPoint(p(t))(p)
    else
      t
  }

  def freeVars(t: Term): Set[Id] = t match {
    case a @ Id(_)       => Set(a)
    case Abstr(id, body) => freeVars(body) - id
    case Applic(a, b)    => freeVars(a) ++ freeVars(b)
    case Empty           => Set()
    case Named(id, term) => freeVars(term)
    case _               => Set()
  }

  //Capture-avoiding substitution
  def substitute(t: Term)(label: (Id, Term)): Term = (t, label) match {
    case (Empty, _)                => Empty
    case (i: Id, (j, k)) if i == j => k
    case (i: Id, _)                => i
    case (a: Atom, _)              => a
    case (Applic(a, b), _)         => Applic(substitute(a)(label), substitute(b)(label))
    case (Abstr(id, body), (x, y)) if id != x && !(freeVars(y)(id)) =>
      Abstr(id, substitute(body)(label))
    case (a @ Abstr(_, _), _) => a
    case _                    => t
  }

}

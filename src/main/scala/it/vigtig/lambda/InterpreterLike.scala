package it.vigtig.lambda

/**
 * @author Hargreaves
 */

trait InterpreterLike extends ParserLike with ASTLike with UnificationLike {

  def listToMap(l: List[(Term, Term)]) =
    l.foldLeft(Map[Term, List[Term]]())(comb)

  def comb(m: Map[Term, List[Term]], t: (Term, Term)) = t match {
    case (a, b) => m + (a -> (b :: m.getOrElse(a, Nil)))
  }

  def interpretProgram(program: String): Option[List[Term]] = {

    parseAll(PRGM, program) match {

      case Success(terms, _) =>

        val groups = terms.groupBy {
          case n: Named   => 'NAMED
          case n: SetType => 'SET
          case _          => 'EXPR
        }

        val nameds = groups.getOrElse('NAMED, Nil)
        val unnameds = groups.getOrElse('EXPR, Nil)
        val sets = groups.getOrElse('SET, Nil)


        val mappings: List[(Term, Term)] = nameds.map {
          case Named(a, b) => a -> b
        }

        val dict: Map[Term, List[Term]] = listToMap(mappings.reverse)

        Some(unnameds.map(t => {
          val res = interpret(t)(dict)
          interpret(res)()
        }))

      case x => println(x); None
    }

  }

  def interpret(t: Term)(context: Map[Term, List[Term]] = Map().withDefaultValue(Nil)): Term =
    fixPoint(t)(x => evalStep(x)(context))

  def evalStep(t: Term)(context: Map[Term, List[Term]]) =
    lookup(fixPoint(t)(t => reducer(t)))(context)


  def show[T](t: T): T = {
    println(t)
    t
  }

  def lookup(t: Term)(context: Map[Term, List[Term]]): Term =
    transform({
      case Applic(f, body) if context.contains(applicant(t)) =>
        val ap = applicant(t)
        val headers = context(ap).map(header)
        val as: List[Term] = args(t).reverse

        val substitutions = headers.map(x => unifyLists(x, as))

        val (terms, subs) = (context(ap), substitutions).zipped.filter((a, b) => b.isDefined)

        terms match {
          case x :: xs =>
            val newBody = stripHeader(x)
            val maybeSubs = for (x <- subs.headOption; y <- x) yield y
            val substitutions = maybeSubs.getOrElse(Map())
            val res = substitutions.foldLeft(newBody)((a, b) => substitute(a)(b))
            res
          case Nil     => Applic(lookup(f)(context), body)
        }

      case Applic(x, y) => Applic(lookup(x)(context), lookup(y)(context))
    })(t)


  def args(t: Term): List[Term] = t match {
    case Applic(x, y) => y :: args(x)
    case _            => Nil
  }

  def applicant(t: Term): Term = transform({
    case Applic(left, right) => applicant(left)
    case Id(_)               => t
  })(t)

  def transform(f: PartialFunction[Term, Term]): PartialFunction[Term, Term] = f orElse {
    case Applic(t, y)       => Applic(transform(f)(t), transform(f)(y))
    case Named(id, body)    => Named(id, transform(f)(body))
    case Abstr(a, b)        => Abstr(transform(f)(a), transform(f)(b))
    case Id(id)             => Id(id)
    case s@SetType(_, _, _) => s
    case Empty              => Empty
    case a: Atom            => a
  }

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
    case App(App(Id("<="), Integer(a)), Integer(b))  => Bit(a <= b)
    case App(App(Bit(p), yes), no)                   => if (p) yes else no
    case App(App(Id("*"), Integer(x)), Integer(y))   => Integer(x * y)
    case App(App(Id("+"), Integer(x)), Integer(y))   => Integer(x + y)
    case App(App(Id("+"), Floating(x)), Floating(y)) => Floating(x + y)
    case App(App(Id("-"), Integer(x)), Integer(y))   => Integer(x - y)
    case App(App(Id("%"), Integer(a)), Integer(b))   => Integer(a % b)
  }

  def reducer = {
    def betaReduce: PartialFunction[Term, Term] = builtIns orElse {
      case Named(id, body)                  => Named(id, betaReduce(body))
      case Applic(Abstr(id: Id, body), rhs) => betaReduce(substitute(body)(id -> rhs))
      case Applic(t, y)                     => Applic(betaReduce(t), betaReduce(y))
      case Abstr(a, b)                      => Abstr(a, betaReduce(b))
      case i@Id(_)                          => i
      case t                                => t
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
    case a@Id(_)             => Set(a)
    case Abstr(id: Id, body) => freeVars(body) - id
    case Applic(a, b)        => freeVars(a) ++ freeVars(b)
    case Empty               => Set()
    case Named(id, term)     => freeVars(term)
    case _                   => Set()
  }

  //Capture-avoiding substitution
  def substitute(t: Term)(label: (Term, Term)): Term = (t, label) match {
    case (Empty, _)                                                   => Empty
    case (i: Id, (j, k)) if i == j                                    => k
    case (i: Id, _)                                                   => i
    case (a: Atom, _)                                                 => a
    case (Applic(a, b), _)                                            => Applic(substitute(a)(label), substitute(b)(label))
    case (Abstr(id: Id, body), (x, y)) if id != x && !freeVars(y)(id) =>
      Abstr(id, substitute(body)(label))
    case (a@Abstr(_, _), _)                                           => a
    case _                                                            => t
  }

}

package it.vigtig.lambda

import it.vigtig.lambda.errors.FraseError
import it.vigtig.lambda.interpreter.LetTransformer
import it.vigtig.lambda.syntax.AST._
import it.vigtig.lambda.syntax.{AST, Parser}

trait Interpreter {
  def interpret(program: String): Either[FraseError, Fragment]
}

case class DefaultInterpreter(parser: Parser, letTransformer: LetTransformer)
    extends Interpreter
    with UnificationLike {

  def listToMap(l: List[(Term, Term)]) =
    l.foldLeft(Map[Term, List[Term]]())(comb)

  def comb(m: Map[Term, List[Term]], t: (Term, Term)) = t match {
    case (a, b) => m + (a -> (b :: m.getOrElse(a, Nil)))
  }

  def interpret(program: String): Either[FraseError, Term] =
    parser
      .parse(program)
      .flatMap(letTransformer.transform)
      .map(term => interpret(term)())

  def interpret(f: Fragment): Fragment = f match {
    case x @ Named(_, _) => x
    case term: Term      => interpret(term)()
  }

  def interpret(t: Term)(context: Map[Term, List[Term]] = Map().withDefaultValue(Nil)): Term =
    fixPoint(t)(x => evalStep(x)(context))

  def evalStep(t: Term)(context: Map[Term, List[Term]]): Term =
    lookup(fixPoint(t)(t => reducer(t)))(context)

  def lookup(t: Term)(context: Map[Term, List[Term]]): Term =
    transform({
      case Application(f, body) if context.contains(applicant(t)) =>
        val ap             = applicant(t)
        val headers        = context(ap).map(header)
        val as: List[Term] = args(t).reverse

        val substitutions = headers.map(x => unifyLists(x, as))

        val (terms, subs) = (context(ap), substitutions).zipped.filter((a, b) => b.isDefined)

        terms match {
          case x :: xs =>
            val newBody       = stripHeader(x)
            val maybeSubs     = for (x <- subs.headOption; y <- x) yield y
            val substitutions = maybeSubs.getOrElse(Map())
            val res           = substitutions.foldLeft(newBody)((a, b) => substitute(a)(b))
            res
//          case Empty => Application(lookup(f)(context), body)
        }

      case Application(x, y) => Application(lookup(x)(context), lookup(y)(context))
    })(t)

  def args(t: Term): List[Term] = t match {
    case Application(x, y) => y :: args(x)
    case _                 => Nil
  }

  def applicant(t: Term): Term =
    transform({
      case Application(left, right) => applicant(left)
      case Identifier(_)            => t
    })(t)

  def transform(f: PartialFunction[Term, Term]): PartialFunction[Term, Term] = f orElse {
    case Application(t, y) => Application(transform(f)(t), transform(f)(y))
//    case Named(id, body)         => Named(id, transform(f)(body))
    case LambdaAbstraction(a, b) => LambdaAbstraction(transform(f)(a), transform(f)(b))
    case Identifier(id)          => Identifier(id)
//    case s @ SetType(_, _, _) => s
    case Empty => Empty
    case a     => a
  }

  val App = Application

  def builtIns: PartialFunction[Term, Term] = {
    case App(App(Identifier("=="), a), b)                    => Bool(a == b)
    case App(App(Identifier("<="), Integer(a)), Integer(b))  => Bool(a <= b)
    case App(App(Bool(p), yes), no)                          => if (p) yes else no
    case App(App(Identifier("*"), Integer(x)), Integer(y))   => Integer(x * y)
    case App(App(Identifier("+"), Integer(x)), Integer(y))   => Integer(x + y)
    case App(App(Identifier("+"), Floating(x)), Floating(y)) => Floating(x + y)
    case App(App(Identifier("-"), Integer(x)), Integer(y))   => Integer(x - y)
    case App(App(Identifier("%"), Integer(a)), Integer(b))   => Integer(a % b)
  }

  def reducer = {
    def betaReduce: PartialFunction[Term, Term] = builtIns orElse {
//      case Named(id, body) =>
//        val reduce: AST.Term = betaReduce(body)
//        Named(id, reduce)
      case Application(LambdaAbstraction(id: Identifier, body), rhs) => betaReduce(substitute(body)(id -> rhs))
      case Application(t, y)                                         => Application(betaReduce(t), betaReduce(y))
      case LambdaAbstraction(a, b)                                   => LambdaAbstraction(a, betaReduce(b))
      case i @ Identifier(_)                                         => i
      case t                                                         => t
    }
    betaReduce
  }

  def fixPoint[T](t: T)(p: T => T): T =
    if (p(t) != t)
      fixPoint(p(t))(p)
    else
      t

  def freeVars(t: Term): Set[Identifier] = t match {
    case a @ Identifier(_)                       => Set(a)
    case LambdaAbstraction(id: Identifier, body) => freeVars(body) - id
    case Application(a, b)                       => freeVars(a) ++ freeVars(b)
    case Empty                                   => Set()
//    case Named(_, term)                          => freeVars(term)
    case _ => Set()
  }

  //Capture-avoiding substitution
  def substitute(t: Term)(label: (Term, Term)): Term = (t, label) match {
    case (Empty, _)                        => Empty
    case (i: Identifier, (j, k)) if i == j => k
    case (i: Identifier, _)                => i
    case (a: Identifier, _)                => a
    case (Application(a, b), _)            => Application(substitute(a)(label), substitute(b)(label))
    case (LambdaAbstraction(id: Identifier, body), (x, y)) if id != x && !freeVars(y)(id) =>
      LambdaAbstraction(id, substitute(body)(label))
    case (a @ LambdaAbstraction(_, _), _) => a
    case _                                => t
  }

}

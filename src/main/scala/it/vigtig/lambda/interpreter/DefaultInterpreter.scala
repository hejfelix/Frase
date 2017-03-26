package it.vigtig.lambda.interpreter

import it.vigtig.lambda.errors.FraseError
import it.vigtig.lambda.syntax.AST._
import it.vigtig.lambda.syntax.Parser
import it.vigtig.lambda.debug
import it.vigtig.lambda.semantic.Keywords

trait Interpreter {
  def interpret(program: String): Either[FraseError, Fragment]
  def interpret(fragment: Fragment, namedTerms: List[Fragment]): Either[FraseError, Term]
}

case class DefaultInterpreter(parser: Parser, letTransformer: LetTransformer, keywords: Keywords)
    extends Interpreter
    with UnificationLike {

  def listToMap(l: List[(Term, Term)]) =
    l.foldLeft(Map[Term, List[Term]]())(comb)

  def comb(m: Map[Term, List[Term]], t: (Term, Term)) = t match {
    case (a, b) => m + (a -> (b :: m.getOrElse(a, Nil)))
  }

  def prettyTrace = debug.trace[Term, String](_.pretty) _

  def interpret(fragment: Fragment, namedTerms: List[Fragment]): Either[FraseError, Term] =
    letTransformer
      .transform(fragment :: namedTerms)
      .map(term => interpret(term)())

  def interpret(program: String): Either[FraseError, Term] =
    parser
      .parse(program)
      .flatMap(letTransformer.transform)
      .map(term => evalStep(term)())

  def interpret(f: Fragment): Fragment = f match {
    case x @ Named(_, _) => x
    case term: Term      => interpret(term)()
  }

  def interpret(t: Term)(context: Map[Term, List[Term]] = Map().withDefaultValue(Nil)): Term =
    fixPoint(t)(x => evalStep(x)(context))

  def evalStep(t: Term)(context: Map[Term, List[Term]] = Map.empty): Term =
    fixPoint(t)(t => { reduce(t) })

  def args(t: Term): List[Term] = t match {
    case Application(x, y) => y :: args(x)
    case _                 => Nil
  }

  def applicant(t: Term): Term =
    t.transform {
      case Application(left, right) => applicant(left)
      case Identifier(_)            => t
    }

  val App = Application

  val yCombinator = keywords.yCombinator
  def builtIns: PartialFunction[Term, Term] = {
    case App(`yCombinator`, f)                               => App(f, App(yCombinator, f))
    case yCombExp @ LambdaAbstraction(`yCombinator`, body)   => Application(body, yCombExp)
    case App(App(Identifier("=="), a), b)                    => Bool(a == b)
    case App(App(Identifier("<="), Integer(a)), Integer(b))  => Bool(a <= b)
    case App(App(Bool(p), yes), no)                          => if (p) yes else no
    case App(App(Identifier("*"), Integer(x)), Integer(y))   => Integer(x * y)
    case App(App(Identifier("+"), Integer(x)), Integer(y))   => Integer(x + y)
    case App(App(Identifier("+"), Floating(x)), Floating(y)) => Floating(x + y)
    case App(App(Identifier("-"), Integer(x)), Integer(y))   => Integer(x - y)
    case App(App(Identifier("%"), Integer(a)), Integer(b))   => Integer(a % b)
  }

  def reduce = {
    def betaReduce: PartialFunction[Term, Term] = builtIns orElse {
//      case Named(id, body) =>
//        val reduce: AST.Term = betaReduce(body)
//        Named(id, reduce)
      case Application(LambdaAbstraction(id, body), rhs) if id == rhs                     => body
      case Application(LambdaAbstraction(id: Identifier, body), rhs) if id != yCombinator =>
//        println(s"Beta reduce id: ${id.pretty}  body: ${body.pretty}   rhs: ${rhs.pretty}")
//        println(s"     with result: ${betaReduce(substitute(body)(id -> rhs)).pretty}")
        betaReduce(substitute(body)(id -> rhs))
      case Application(t, y)       => Application(betaReduce(t), betaReduce(y))
      case LambdaAbstraction(a, b) => LambdaAbstraction(a, betaReduce(b))
      case i @ Identifier(_)       => i
      case t                       => t
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

package com.lambdaminute.interpreter

import com.lambdaminute.errors.FraseError
import com.lambdaminute.syntax.AST._
import com.lambdaminute.syntax.{AST, Parser}
import com.lambdaminute.debug
import com.lambdaminute.semantic.Keywords

case class DefaultInterpreter(parser: Parser, letTransformer: LetTransformer, keywords: Keywords) extends Interpreter {

  def listToMap(l: List[(Term, Term)]): Map[Term, List[Term]] =
    l.foldLeft(Map[Term, List[Term]]())(comb)

  def comb(m: Map[Term, List[Term]], t: (Term, Term)): Map[Term, List[Term]] = t match {
    case (a, b) => m + (a -> (b :: m.getOrElse(a, Nil)))
  }

  val prettyTrace: Term => Term = debug.trace[Term, String](_.pretty)

  def interpret(fragment: Fragment, namedTerms: List[Fragment]): Either[FraseError, Term] =
    letTransformer
      .transform(fragment :: namedTerms)
      .map(term => interpret(term))

  def interpret(program: String): Either[FraseError, Term] =
    parser
      .parse(program)
      .flatMap(letTransformer.transform)
      .map(term => evalStep(term))

  def interpret(f: Fragment): Fragment = f match {
    case x @ Named(_, _) => x
    case term: Term      => interpret(term)
  }

  def interpret(t: Term): Term =
    fixPoint(t)(x => evalStep(x))

  def evalStep(t: Term): Term =
    fixPoint(t)(t => { reduce(t) })

  def args(t: Term): List[Term] = t match {
    case Application(x, y) => y :: args(x)
    case _                 => Nil
  }

  def applicant(t: Term): Term =
    t.transform {
      case Application(left, _) => applicant(left)
      case Identifier(_)        => t
    }

  val App: AST.Application.type = Application

  val yCombinator: Identifier = keywords.yCombinator
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

  def reduce: PartialFunction[Term, Term] = {
    def betaReduce: PartialFunction[Term, Term] = builtIns orElse {
      case Application(LambdaAbstraction(id, body), rhs) if id == rhs => body
      case Application(LambdaAbstraction(id: Identifier, body), rhs) if id != yCombinator =>
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
    case _                                       => Set()
  }

  //Capture-avoiding substitution
  def substitute(t: Term)(label: (Term, Term)): Term = (t, label) match {
    case (Empty, _)                        => Empty
    case (i: Identifier, (j, k)) if i == j => k
    case (i: Identifier, _)                => i
    case (Application(a, b), _)            => Application(substitute(a)(label), substitute(b)(label))
    case (LambdaAbstraction(id: Identifier, body), (x, y)) if id != x && !freeVars(y)(id) =>
      LambdaAbstraction(id, substitute(body)(label))
    case (a @ LambdaAbstraction(_, _), _) => a
    case _                                => t
  }

}

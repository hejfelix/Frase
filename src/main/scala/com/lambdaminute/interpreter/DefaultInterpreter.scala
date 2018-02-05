package com.lambdaminute.interpreter

import com.lambdaminute.ast.AST
import com.lambdaminute.errors.FraseError
import com.lambdaminute.ast.AST._
import com.lambdaminute.grammar.Parser
import com.lambdaminute.debug
import com.lambdaminute.semantic.Keywords

case class DefaultInterpreter(parser: Parser, letTransformer: LetTransformer, keywords: Keywords)
    extends Interpreter
    with FixPoint {

  implicit class TermSyntax(t: Term) {
    def interpret = DefaultInterpreter.this.interpret(t)
    def reduce    = DefaultInterpreter.this.reduce(t)
  }

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
      .map(evalStep)

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
        betaReduce(body.substitute(id -> rhs))
      case Application(t, y)       => Application(betaReduce(t), betaReduce(y))
      case LambdaAbstraction(a, b) => LambdaAbstraction(a, betaReduce(b))
      case i @ Identifier(_)       => i
      case t                       => t
    }
    betaReduce
  }

}

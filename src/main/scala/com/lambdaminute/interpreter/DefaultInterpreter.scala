package com.lambdaminute.interpreter

import com.lambdaminute.ast.AST
import com.lambdaminute.errors.FraseError
import com.lambdaminute.ast.AST._
import com.lambdaminute.grammar.Parser
import com.lambdaminute.debug
import com.lambdaminute.semantic.Keywords

case class DefaultInterpreter(parser: Parser,
                              letTransformer: LetTransformer,
                              keywords: Keywords,
                              builtIns: Keywords => PartialFunction[Term, Term])
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

  val App: AST.Application.type = Application

  def reduce: PartialFunction[Term, Term] = {
    def betaReduce: PartialFunction[Term, Term] = builtIns(keywords) orElse {
      case Application(LambdaAbstraction(id, body), rhs) if id == rhs => body
      case Application(LambdaAbstraction(id: Identifier, body), rhs) if id != keywords.yCombinator =>
        betaReduce(body.substitute(id -> rhs))
      case Application(t, y)       => Application(betaReduce(t), betaReduce(y))
      case LambdaAbstraction(a, b) => LambdaAbstraction(a, betaReduce(b))
      case i @ Identifier(_)       => i
      case t                       => t
    }
    betaReduce
  }

}

package com.lambdaminute.frase.calculus.interpreter

import com.lambdaminute.frase.calculus.ast.Ast
import com.lambdaminute.frase.calculus.ast.Ast._
import com.lambdaminute.frase.calculus.debug
import com.lambdaminute.frase.calculus.errors.FraseError
import com.lambdaminute.frase.calculus.grammar.Parser
import com.lambdaminute.frase.calculus.semantic.Keywords
import cats.implicits._

case class DefaultInterpreter(parser: Parser, keywords: Keywords, builtIns: Keywords => BetaReduction)
    extends Interpreter
    with FixPoint {

  implicit class TermSyntax(t: Term) {
    def interpret: Either[FraseError, Term] = DefaultInterpreter.this.interpret(t)
    def reduce                              = DefaultInterpreter.this.reduce(t)
  }

  def listToMap(l: List[(Term, Term)]): Map[Term, List[Term]] =
    l.foldLeft(Map[Term, List[Term]]())(comb)

  def comb(m: Map[Term, List[Term]], t: (Term, Term)): Map[Term, List[Term]] = t match {
    case (a, b) => m + (a -> (b :: m.getOrElse(a, Nil)))
  }

  val prettyTrace: Term => Term = debug.trace[Term, String](_.pretty)

  def interpret(program: String): Either[FraseError, Term] =
    parser
      .parse(program)
      .flatMap(interpret)

  def interpret(t: Term): Either[FraseError, Term] =
    interpretScan(t).last // yiikes

  val App: Ast.Application.type = Application

  def reduce: BetaReduction = {
    def betaReduce: BetaReduction = builtIns(keywords) orElse {
      case Application(LambdaAbstraction(id: Identifier, body), rhs) if id != keywords.yCombinator =>
        body.substitute(id -> rhs)
      case Application(t, y)       => Application(betaReduce(t), betaReduce(y))
      case LambdaAbstraction(a, b) => LambdaAbstraction(a, betaReduce(b))
      case i @ Identifier(_)       => i
      case t                       => t
    }
    betaReduce
  }

  override def interpretScan(term: Term): Stream[Either[FraseError, Term]] = {
    def steps: Stream[Term] = Stream.iterate(term)(reduce)

    term.asRight[FraseError] #:: steps
      .zip(steps.tail)
      .takeWhile {
        case (a, b) => a != b
      }
      .map(_._2)
      .map(Right.apply)
  }

  override def interpretScan(program: String): Stream[Either[FraseError, Term]] =
    parser.parse(program) match {
      case error @ Left(_) => Stream(error)
      case Right(t)        => interpretScan(t)
    }

}

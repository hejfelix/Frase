package com.lambdaminute.frase.calculus.interpreter

import com.lambdaminute.frase.calculus.ast.AST
import com.lambdaminute.frase.calculus.ast.AST._
import com.lambdaminute.frase.calculus.debug
import com.lambdaminute.frase.calculus.errors.FraseError
import com.lambdaminute.frase.calculus.grammar.Parser
import com.lambdaminute.frase.calculus.semantic.Keywords
import cats.implicits._

case class DefaultInterpreter(parser: Parser,
                              keywords: Keywords,
                              builtIns: Keywords => BetaReduction)
    extends Interpreter
    with FixPoint {

  implicit class TermSyntax(t: Term) {
    def interpret: Either[FraseError, Term] = DefaultInterpreter.this.interpret(t)
    def reduce                              = DefaultInterpreter.this.reduce(t.nextAvailableId -> t)
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

  val App: AST.Application.type = Application

  def reduce: BetaReduction = {
    def betaReduce: BetaReduction = builtIns(keywords) orElse {
      case (nextId, Application(LambdaAbstraction(id, body), rhs)) if id == rhs => (nextId, body)
      case (nextId, Application(LambdaAbstraction(id: Identifier, body), rhs))
          if id != keywords.yCombinator =>
        betaReduce(nextId -> body.substitute(id -> rhs))
      case (nextId, Application(t, y)) =>
        val (nextLeft, left)   = betaReduce(nextId   -> t)
        val (nextRight, right) = betaReduce(nextLeft -> y)
        (nextRight, Application(left, right))
      case (nextId, LambdaAbstraction(a, b)) =>
        val (nextBody, body) = betaReduce(nextId -> b)
        (nextBody, LambdaAbstraction(a, body))
      case (nextId, i @ Identifier(_)) => (nextId, i)
      case t                           => t
    }
    betaReduce
  }

  override def interpretScan(term: Term): Stream[Either[FraseError, Term]] = {
    def steps: Stream[Term] =
      Stream
        .iterate(term.nextAvailableId -> term) {
          case (nextId, t) =>
            println(s"Next available id:${nextId},  ${nextId.pretty}")
            reduce(nextId -> t)
        }
        .map(_._2)

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

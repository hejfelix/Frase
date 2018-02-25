package com.lambdaminute.frase.calculus.interpreter

import com.lambdaminute.frase.calculus.ast.AST
import com.lambdaminute.frase.calculus.ast.AST._
import com.lambdaminute.frase.calculus.debug
import com.lambdaminute.frase.calculus.errors.FraseError
import com.lambdaminute.frase.calculus.grammar.Parser
import com.lambdaminute.frase.calculus.semantic.Keywords
import cats.implicits._

case class DefaultInterpreter(parser: Parser, keywords: Keywords, builtIns: Keywords => PartialFunction[Term, Term])
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

  def interpret(program: String): Either[FraseError, Term] =
    parser
      .parse(program)
      .flatMap(interpret)

  def interpret(t: Term): Either[FraseError, Term] =
    Right(fixPoint(t)(t => reduce(t)))

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

  override def interpretScan(term: Term): List[Either[FraseError, Term]] = {
    def steps: Stream[Term] = Stream.iterate(term) { t =>
      reduce(t)
    }

    term.asRight[FraseError] :: steps
      .zip(steps.tail)
      .takeWhile {
        case (a, b) => a != b
      }
      .map(_._2)
      .map(Right.apply)
      .toList
  }

  override def interpretScan(program: String): List[Either[FraseError, Term]] =
    parser.parse(program) match {
      case error @ Left(_) => List(error)
      case Right(t)        => interpretScan(t)
    }

}

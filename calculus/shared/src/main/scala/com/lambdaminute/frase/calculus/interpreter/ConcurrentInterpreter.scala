package com.lambdaminute.frase.calculus.interpreter

import cats.data.EitherT
import cats.effect.Concurrent
import cats.implicits._
import com.lambdaminute.frase.calculus.ast.Ast.{Application, Identifier, LambdaAbstraction, Term}
import com.lambdaminute.frase.calculus.errors.FraseError
import com.lambdaminute.frase.calculus.grammar.Parser
import com.lambdaminute.frase.calculus.interpreter.interpreter.BetaReduction
import com.lambdaminute.frase.calculus.semantic.Keywords

class ConcurrentInterpreter[F[_]](parser: Parser, keywords: Keywords, builtIns: Keywords => BetaReduction)(
    implicit F: Concurrent[F]) {

  def interpret(program: String): EitherT[F, FraseError, Term] =
    EitherT(F.pure(parser.parse(program)))
      .flatMap(interpret)

  def interpret(t: Term): EitherT[F, FraseError, Term] =
    EitherT.right(interpretScan(t).map(_.last))

  type BetaReductionF = PartialFunction[Term, F[Term]]

  private val builtInsF: PartialFunction[Term, F[Term]] = builtIns(keywords).andThen(F.pure)

  def reduce: BetaReductionF = {
    def betaReduce: BetaReductionF = builtInsF orElse {
      case Application(LambdaAbstraction(id: Identifier, body), rhs) if id != keywords.yCombinator =>
        F.pure(body.substitute(id -> rhs))
      case Application(t, y) =>
        for {
          leftFiber  <- F.start { betaReduce(t) }
          rightFiber <- F.start { betaReduce(y) }
          leftTerm   <- leftFiber.join
          rightTerm  <- rightFiber.join
        } yield Application(leftTerm, rightTerm)
      case LambdaAbstraction(a, b) => betaReduce(b).map(LambdaAbstraction(a, _))
      case i @ Identifier(_)       => F.pure(i)
      case t                       => F.pure(t)
    }
    betaReduce
  }

  private def interpretFStream(term: Term): F[Stream[Term]] =
    for {
      head <- reduce(term)
      next <- reduce(head)
      rest = if (head == next) F.pure(Stream.empty[Term]) else interpretFStream(next)
      continuation <- rest
    } yield (head #:: next #:: continuation)

  def interpretScan(term: Term): F[Stream[Term]] =
    interpretFStream(term)

}

package com.lambdaminute.frase.calculus.interpreter

import cats.data.Writer
import com.lambdaminute.frase.calculus.ast.Ast.Term
import com.lambdaminute.frase.calculus.errors.FraseError

trait Interpreter {
  def interpret(program: String): Either[FraseError, Term]
  def interpret(term: Term): Either[FraseError, Term]
  def interpretScan(term: Term): Stream[Either[FraseError, Term]]
  def interpretScan(program: String): Stream[Either[FraseError, Term]]
  def traceInterpret(term: Term): Stream[(List[Trace], Term)]
  def traceInterpret(program: String): Either[FraseError, Stream[(List[Trace], Term)]]
}

package object interpreter {

  type BetaReduction = PartialFunction[Term, Term]

  type TracingBetaReduction = PartialFunction[Term, Writer[List[Trace], Term]]

}

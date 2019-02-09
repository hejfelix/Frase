package com.lambdaminute.frase.calculus.interpreter

import cats.Id
import cats.data.{Writer, WriterT}
import cats.implicits._
import com.lambdaminute.frase.calculus.ast.Ast
import com.lambdaminute.frase.calculus.ast.Ast._
import com.lambdaminute.frase.calculus.debug
import com.lambdaminute.frase.calculus.errors.FraseError
import com.lambdaminute.frase.calculus.grammar.Parser
import com.lambdaminute.frase.calculus.interpreter.interpreter.{BetaReduction, TracingBetaReduction}
import com.lambdaminute.frase.calculus.semantic.Keywords

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

  def reduce: BetaReduction = builtIns(keywords) orElse {
    case Application(LambdaAbstraction(id: Identifier, body), rhs) if id != keywords.yCombinator =>
      body.substitute(id -> rhs)
    case Application(t, y)       => Application(reduce(t), reduce(y))
    case LambdaAbstraction(a, b) => LambdaAbstraction(a, reduce(b))
    case i @ Identifier(_)       => i
    case t                       => t
  }

  def traceReduce: TracingBetaReduction = {

    val totalBuiltin: Term => Option[(Term, Term)] =
      builtIns(keywords).lift.product(identity).map(tuple => tuple._1.map(_ -> tuple._2))

    // Ugh...
    val traceBuiltin: Term => Option[WriterT[Id, List[Trace], Term]] = totalBuiltin.map {
      case Some((result, original)) =>
        println(s"Trace builtin: ${original} ${result}")
        Some(Writer(List[Trace](BuiltInTrace(original, result)), result))
      case None =>
        println(s"Trace builtin: None")
        None
    }

    val partialTraceBuiltin: TracingBetaReduction = Function.unlift(traceBuiltin)

    val doTraceReduce: TracingBetaReduction = {
      case Application(LambdaAbstraction(id: Identifier, body), rhs) if id != keywords.yCombinator =>
        val result = body.substitute(id -> rhs)
        val trace  = ApplicationTrace(id, rhs, body)
        Writer(List(trace), result)
      case Application(t, y) =>
        for {
          leftReduced  <- traceReduce(t)
          rightReduced <- traceReduce(y)
        } yield Application(leftReduced, rightReduced)
      case LambdaAbstraction(a, b) => traceReduce(b).map(reducedB => LambdaAbstraction(a, reducedB))
      case i @ Identifier(_)       => Writer(Nil, i)
      case t                       => Writer(Nil, t)
    }

    partialTraceBuiltin orElse doTraceReduce
  }

  override def traceInterpret(term: Term): Stream[(List[Trace], Term)] =
    (List.empty[Trace], term) #:: Stream
      .iterate(traceReduce(term).run) {
        case (traces, term) =>
          println((traces, term.pretty))
          traceReduce(term).run
      }
      .takeWhile(_._1.nonEmpty)

  override def traceInterpret(program: String): Either[FraseError, Stream[(List[Trace], Term)]] =
    parser.parse(program).map(term => traceInterpret(term))

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
      case Right(t)        => interpretScan(t)
      case error @ Left(_) => Stream(error)
    }

}

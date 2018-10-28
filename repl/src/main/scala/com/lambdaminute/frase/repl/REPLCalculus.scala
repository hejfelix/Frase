package com.lambdaminute.frase.repl

import com.lambdaminute.frase.calculus.ast.Ast.Term
import com.lambdaminute.frase.calculus.errors.FraseError
import com.lambdaminute.frase.calculus.grammar.{DefaultLexer, DefaultParser}
import com.lambdaminute.frase.calculus.interpreter.{DefaultBuiltins, DefaultInterpreter, Interpreter}
import com.lambdaminute.frase.calculus.semantic.DefaultKeywords

object REPLCalculus extends App {

  def defaultKeywords = DefaultKeywords()
  def lexer           = DefaultLexer()
  def parser          = DefaultParser(lexer)
  def defaultBuiltins = DefaultBuiltins.builtIns
  def interpreter: Interpreter =
    DefaultInterpreter(parser, defaultKeywords, defaultBuiltins)

  sealed trait Result
  case class TermResult(t: Term) extends Result
  case object ExitResult         extends Result

  /*
  Processing
   */
  def processStandaloneExpression(term: Term): Either[FraseError, Result] =
    interpreter.interpret(term) match {
      case Right(result) => processEvaluationSuccess(result)
      case Left(err)     => processParsingError(err)
    }

  private def processEvaluationSuccess(result: Term): Either[FraseError, Result] = {
    println(result.pretty)
    Right(TermResult(result))
  }

  private def processParsingError(err: FraseError): Either[FraseError, Result] = {
    println(err)
    Left(err)
  }

  def reactToLine(): Either[FraseError, Result] =
    io.StdIn.readLine("Frase>") match {
      case ":exit"      => Right(ExitResult)
      case line: String => parseLine(line)
    }

  private def parseLine(line: String): Either[FraseError, Result] =
    parser.parseFragment(line) match {
      case Right(term) => processStandaloneExpression(term)
      case Left(err)   => processParsingError(err)
    }

  Stream
    .continually(reactToLine)
    .takeWhile(_ != Right(ExitResult))
    .force

}

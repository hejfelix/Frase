package com.lambdaminute

import cats.data.State
import com.lambdaminute.ast.AST.{Empty, Fragment, Named, Term}
import com.lambdaminute.errors.FraseError
import com.lambdaminute.grammar.{DefaultLexer, DefaultParser}
import com.lambdaminute.interpreter.{DefaultBuiltins, DefaultInterpreter, DefaultLetTransformer, Interpreter}
import com.lambdaminute.semantic.DefaultKeywords

object REPL extends App {

  def defaultKeywords          = DefaultKeywords()
  def lexer                    = DefaultLexer()
  def parser                   = DefaultParser(lexer)
  def letTransformer           = DefaultLetTransformer(defaultKeywords)
  def defaultBuiltins          = DefaultBuiltins.builtIns
  def interpreter: Interpreter = DefaultInterpreter(parser, letTransformer, defaultKeywords, defaultBuiltins)

  /*
  Processing
   */
  def processStandaloneExpression(term: Fragment, namedTerms: List[Fragment]) =
    interpreter.interpret(term, namedTerms) match {
      case Right(result) => processEvaluationSuccess(namedTerms, result)
      case Left(err)     => processParsingError(namedTerms, err)
    }

  private def processEvaluationSuccess(namedTerms: List[Fragment], result: Term): (List[Fragment], Option[Term]) = {
    println(result.pretty)
    (namedTerms, Option(result))
  }

  private def processParsingError(namedTerms: List[Fragment], err: FraseError): (List[Fragment], Option[Term]) = {
    println(err)
    (namedTerms, Option(Empty))
  }

  private def processTermNaming(namedTerms: List[Fragment], n: Named): (List[Fragment], Option[Term]) = {
    println(s"${n.lhs.pretty}: ${n.rhs.pretty}")
    (n :: namedTerms, Option(Empty))
  }

  /*
  Reading and parsing
   */
  def readNextLine(): State[List[Fragment], Option[Term]] = State(reactToLine)

  def reactToLine(namedTerms: List[Fragment]) =
    io.StdIn.readLine("Frase>") match {
      case ":exit" => (namedTerms, None)
      case line    => parseLine(namedTerms, line)
    }

  private def parseLine(namedTerms: List[Fragment], line: String) =
    parser.parseFragment(line) match {
      case Right(n: Named) => processTermNaming(namedTerms, n)
      case Right(term)     => processStandaloneExpression(term, namedTerms)
      case Left(err)       => processParsingError(namedTerms, err)
    }

  def results(accum: List[Term]): State[List[Fragment], List[Term]] =
    readNextLine().flatMap {
      case Some(t) => results(t :: accum)
      case None    => State[List[Fragment], List[Term]](x => (x, accum))
    }

  results(Nil).run(Nil).value

}

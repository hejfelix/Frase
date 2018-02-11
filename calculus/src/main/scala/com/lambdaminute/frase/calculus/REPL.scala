package com.lambdaminute.frase.calculus
import com.lambdaminute.frase.calculus.ast.AST.Term
import com.lambdaminute.frase.calculus.errors.FraseError
import com.lambdaminute.frase.calculus.grammar.{DefaultLexer, DefaultParser}
import com.lambdaminute.frase.calculus.interpreter.{DefaultBuiltins, DefaultInterpreter, Interpreter}
import com.lambdaminute.frase.calculus.semantic.DefaultKeywords

object REPL extends App {

  def defaultKeywords = DefaultKeywords()
  def lexer           = DefaultLexer()
  def parser          = DefaultParser(lexer)
  def defaultBuiltins = DefaultBuiltins.builtIns
  def interpreter: Interpreter =
    DefaultInterpreter(parser, defaultKeywords, defaultBuiltins)

  /*
  Processing
   */
  def processStandaloneExpression(term: Term): Option[Term] =
    interpreter.interpret(term) match {
      case Right(result) => processEvaluationSuccess(result)
      case Left(err)     => processParsingError(err)
    }

  private def processEvaluationSuccess(result: Term): Option[Term] = {
    println(result.pretty)
    Option(result)
  }

  private def processParsingError(err: FraseError): Option[Term] = {
    println(err)
    None
  }

  def reactToLine(): Option[Term] =
    io.StdIn.readLine("Frase>") match {
      case ":exit"      => None
      case line: String => parseLine(line)
    }

  private def parseLine(line: String): Option[Term] =
    parser.parseFragment(line) match {
      case Right(term) => processStandaloneExpression(term)
      case Left(err)   => processParsingError(err)
    }

}

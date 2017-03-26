package it.vigtig.lambda

import cats.data.State
import it.vigtig.lambda.errors.FraseError
import it.vigtig.lambda.interpreter.{DefaultInterpreter, DefaultLetTransformer, Interpreter}
import it.vigtig.lambda.semantic.DefaultKeywords
import it.vigtig.lambda.syntax.AST.{Empty, Fragment, Named, Term}
import it.vigtig.lambda.syntax.{DefaultLexer, DefaultParser}

object REPL extends App {

  val defaultKeywords          = DefaultKeywords()
  val lexer                    = DefaultLexer()
  val parser                   = DefaultParser(lexer)
  val letTransformer           = DefaultLetTransformer(defaultKeywords)
  val interpreter: Interpreter = DefaultInterpreter(parser, letTransformer, defaultKeywords)

  def processStandaloneExpression(term: Fragment, namedTerms: List[Fragment]) =
    interpreter.interpret(term, namedTerms) match {
      case Right(result) => processEvaluationSuccess(namedTerms, result)
      case Left(err)     => processParsingError(namedTerms, err)
    }

  private def processEvaluationSuccess(namedTerms: List[Fragment], result: Term) = {
    println(result.pretty)
    (namedTerms, result)
  }

  private def processParsingError(namedTerms: List[Fragment], err: FraseError) = {
    println(err)
    (namedTerms, Empty)
  }

  def readNextLine(): State[List[Fragment], Term] =
    State(namedTerms => {
      parser.parseFragment(io.StdIn.readLine("Frase>")) match {
        case Right(n: Named) => processTermNaming(namedTerms, n)
        case Right(term)     => processStandaloneExpression(term, namedTerms)
        case Left(err)       => processParsingError(namedTerms, err)
      }
    })

  private def processTermNaming(namedTerms: List[Fragment], n: Named) = {
    println(s"${n.lhs.pretty}: ${n.rhs.pretty}")
    (n :: namedTerms, Empty)
  }

  val results: State[List[Fragment], List[Term]] =
    for {
      term  <- readNextLine()
      terms <- results
    } yield term :: terms

  results.run(Nil).value

  val fac =
    """|fac = yCombinator . f . n . (<= n 1) 1 (* n (f (- n 1)) )
      |fac 3
    """.stripMargin

  println(interpreter.interpret(fac))

  println(parser.parseFragment("yCombinator . f . n . (<= n 1) 1 (* n (f (- n 1)) )").map(_.pretty))


}

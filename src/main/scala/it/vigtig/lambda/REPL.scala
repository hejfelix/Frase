package it.vigtig.lambda

import it.vigtig.lambda.interpreter.DefaultLetTransformer
import it.vigtig.lambda.syntax.{DefaultLexer, DefaultParser}

object REPL extends App {

  val lexer          = DefaultLexer()
  val parser         = DefaultParser(lexer)
  val letTransformer = DefaultLetTransformer()
  val interpreter    = DefaultInterpreter(parser, letTransformer)

  val program =
    """|x = 4
       |* (+ 1 x) 3
    """.stripMargin

  println(interpreter.interpret(program))

}

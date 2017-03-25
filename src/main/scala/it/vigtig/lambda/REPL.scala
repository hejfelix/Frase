package it.vigtig.lambda

import it.vigtig.lambda.syntax.{DefaultLexer, DefaultParser}

object REPL extends App {

  val lexer       = DefaultLexer()
  val parser      = DefaultParser(lexer)
  val interpreter = DefaultInterpreter(parser)

  val program =
    """|* (+ 1 2) 3
    """.stripMargin

  println(interpreter.interpret(program))

}

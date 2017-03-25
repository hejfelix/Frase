package it.vigtig.lambda

import it.vigtig.lambda.errors.FraseError
import it.vigtig.lambda.interpreter.DefaultLetTransformer
import it.vigtig.lambda.syntax.AST.Term
import it.vigtig.lambda.syntax.{AST, DefaultLexer, DefaultParser}

object REPL extends App {

  val lexer          = DefaultLexer()
  val parser         = DefaultParser(lexer)
  val letTransformer = DefaultLetTransformer()
  val interpreter    = DefaultInterpreter(parser, letTransformer)

  val program =
    """|plus = a . b . + a b
      |x = 4
      |y = 3
      |* (plus 1 x) y
    """.stripMargin

  val fac =
    """|fac = n . (<= n 1) 1 (* n ((y fac) (- n 1)))
      |fac 3
    """.stripMargin

  val ycomb =
    """|(x . f (x x)) (x . f (x x))
    """.stripMargin

  val ytest =
    """|fac = y . f . n . (<= n 1) 1 (* n (f (- n 1)))
       |fac 4""".stripMargin
  val ytest2 = "( 0 . 1 ) 0"

  val ytest3 =
    """y (f . + 1 f)""".stripMargin

  val term: Either[FraseError, AST.Term] = parser.parse(ytest).flatMap(letTransformer.transform)

  println(term)
  for (i <- 1 to 20) {
    println(s"$i:   ${term.map(reduce(i)).map(_.pretty)}")
  }


  def reduceTwice(t: Term) = interpreter.reduce(interpreter.reduce(t))

  def reduce(times: Int = 1)(t: Term): Term = times match {
    case 0 => t
    case n => reduce(n - 1)(interpreter.reduce(t))
  }
}


package com.lambdaminute.frase.interpreter

import com.lambdaminute.frase.calculus.ast.Ast
import com.lambdaminute.frase.calculus.errors.FraseError
import com.lambdaminute.frase.calculus.grammar.{DefaultLexer, DefaultParser}
import com.lambdaminute.frase.calculus.interpreter.{DefaultBuiltins, DefaultInterpreter, Trace}
import com.lambdaminute.frase.calculus.semantic.DefaultKeywords
import org.scalatest.WordSpec

class DefaultInterpreterSpec extends WordSpec {

  val interpreter =
    new DefaultInterpreter(DefaultParser(DefaultLexer()), DefaultKeywords(), DefaultBuiltins.builtIns)

  "Iterpreter" should {
    "trace interpretation" in {
      println("Testing")
      val result: Either[FraseError, Stream[(List[Trace], Ast.Term)]] = interpreter.traceInterpret("(x . + 40 x) 2")

      val last = result.right.get.takeRight(1).head

      println(last)

    }
  }

}

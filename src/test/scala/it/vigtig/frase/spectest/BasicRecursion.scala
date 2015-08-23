/**
 * @author Felix
 */
package it.vigtig.frase.spectest

import org.scalacheck.Gen
import org.scalatest.Matchers.convertToAnyShouldWrapper
import org.scalatest.PropSpec
import org.scalatest.prop.GeneratorDrivenPropertyChecks

import it.vigtig.lambda.AST.Integer
import it.vigtig.lambda.AST.Term
import it.vigtig.lambda.InterpreterLike
import it.vigtig.lambda.ParserLike

class BasicRecursion extends PropSpec
    with InterpreterLike
    with ParserLike
    with ASTGenerators
    with GeneratorDrivenPropertyChecks {

  def parseProgramTest(l: String)(b: Term => Unit) = {
    interpretProgram(l) match {
      case Some(ts) => ts map (t => b(interpret(t)()))
      case None     => ???
    }
  }

  def fib(n: Int): Int = if (n < 2) n else fib(n - 1) + fib(n - 2)
  val FIB_FUNC = 
"""
fib = n . if (<= n 1) (n) ((+ (fib (- n 2)) (fib (- n 1))))
"""
  
  property("Fibonacci sequence") {
    forAll(Gen.choose(0, 15)) {
      (n: Int) => parseProgramTest(FIB_FUNC + s"fib $n")(_ shouldBe Integer(fib(n)))
    }
  }
  
  
  def fac(n:Int) = (1 to n).product
  val FAC_FUNC = 
"""
fac = n . if (<= n 1) (1) (* (n) (fac (- n 1)))
"""
  
  property("Factorial function") {
    forAll(Gen.choose(0, 1337)) {
      (n: Int) => parseProgramTest(FAC_FUNC + s"fac $n")(_ shouldBe Integer(fac(n)))
    }
  }
  
}
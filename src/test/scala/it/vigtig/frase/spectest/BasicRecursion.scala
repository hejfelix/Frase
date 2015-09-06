/**
 * @author Felix
 */
package it.vigtig.frase.spectest

import org.scalacheck.Gen
import org.scalatest.Matchers.convertToAnyShouldWrapper
import org.scalatest.PropSpec
import org.scalatest.prop.GeneratorDrivenPropertyChecks

import it.vigtig.lambda.InterpreterLike
import it.vigtig.lambda.ParserLike

class BasicRecursion extends PropSpec
    with InterpreterLike
    with ParserLike
    with ASTGenerators
    with GeneratorDrivenPropertyChecks 
    {

  def parseProgramTest(l: String)(b: Term => Unit) = {
    interpretProgram(l) match {
      case Some(ts) => ts.filter(_!=Empty).map(t => b(interpret(t)()))
      case None     => ???
    }
  }

  def fib(n: Int): Int = if (n < 2) n else fib(n - 1) + fib(n - 2)
  val FIB_FUNC = 
"""
fib = n . (<= n 1) (n) ((+ (fib (- n 2)) (fib (- n 1))))
"""
  
  property("Fibonacci sequence") {
    forAll(Gen.choose(1, 15)) {
      (n: Int) => parseProgramTest(FIB_FUNC + s"\nfib $n")(_ shouldBe Integer(fib(n)))
    }
  }
  
  
  val ALT_FIB_FUNC = 
"""
fib = 1 . 1
fib = 2 . 1
fib = n . + (fib (- n 1)) (fib (- n 2))
"""
  
  property("Alternative definition Fibonacci sequence") {
    forAll(Gen.choose(1, 15)) {
      (n: Int) => parseProgramTest(ALT_FIB_FUNC + s"\nfib $n")(_ shouldBe Integer(fib(n)))
    }
  }  
  
  
  def fac(n:Int) = (1 to n).product
  val FAC_FUNC = 
"""
fac = n . (<= n 1) (1) (* (n) (fac (- n 1)))
"""
  
  property("Factorial function") {
    forAll(Gen.choose(1, 20)) {
      (n: Int) => parseProgramTest(FAC_FUNC + s"\nfac $n")(_ shouldBe Integer(fac(n)))
    }
  }



}
/**
 * @author Felix
 */
package it.vigtig.frase.spectest

import java.lang.Integer

import it.vigtig.lambda.ParserTest._
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

  val FIB_DEF =
    """
  fib = n . (<= n 2) (1) ((+ (fib (- n 2)) (fib (- n 1))))""".stripMargin

  def fib(n: Int): Int = if (n < 2) n else fib(n - 1) + fib(n - 2)

  
  property("Fibonacci sequence") {
    forAll(Gen.choose(1, 6)) {
      (n: Int) => parseProgramTest(FIB_DEF + s"\nfib $n")(_ shouldBe Integer(fib(n)))
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
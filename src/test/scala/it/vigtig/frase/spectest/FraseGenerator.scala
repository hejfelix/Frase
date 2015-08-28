/**
 * @author Felix
 */
package it.vigtig.frase.spectest

import org.scalatest.Matchers.convertToAnyShouldWrapper
import org.scalatest.PropSpec
import org.scalatest.prop.GeneratorDrivenPropertyChecks

import it.vigtig.lambda.InterpreterLike
import it.vigtig.lambda.ParserLike

class FraseGenerator extends PropSpec
    with InterpreterLike
    with ParserLike
    with ASTGenerators
    with GeneratorDrivenPropertyChecks {
  
  property("Ids are leaves") {
    forAll {
      (id: Id) =>
        size(id) shouldEqual 1
    }
  }

  property("Atoms cannot be reduced") {
    forAll {
      (a: Atom) =>
        interpret(a)() shouldEqual a
    }
  }

  def parseLineTest(l:String)(b: Term => Unit){
    parseAll(LINE,l) match {
      case Success(ast,_) => b(interpret(ast)())
      case pr => fail("Term failed to parse"+pr)
    }
  }
    
  property("Constant-function reduces to constant") {
    forAll  {
      (x: String, y: String) => 
        whenever(x!=y){
          parseLineTest(s"($x . $y) $x") (_ shouldBe Id(y))
        }
    }
  }    
  
  property("Identity-function reduces to identity") {
    forAll {
      (x: String, y: String) => 
        whenever(x!=y){
          parseLineTest(s"($x . $x) $y") (_ shouldBe Id(y))
        }
    }
  }

  property("'false' returns 2nd argument") {
    forAll {
      (x: String, y: String) => 
        whenever(x!=y){
          parseLineTest(s"if false $x $y") (_ shouldBe Id(y))
        }
    }
  }  
  
  property("'true' returns 1st argument") {
    forAll {
      (x: String, y: String) => 
        whenever(x!=y){
          parseLineTest(s"if true $x $y") (_ shouldBe Id(x))
        }
    }
  }


}
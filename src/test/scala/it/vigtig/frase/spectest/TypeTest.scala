package it.vigtig.frase.spectest

import it.vigtig.lambda.ParserLike
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import it.vigtig.lambda.InterpreterLike
import org.scalatest.PropSpec
import it.vigtig.lambda.HindleyMilnerLike
import org.scalatest.Matchers.convertToAnyShouldWrapper
/*
 * @author Hargreaves
 */
class TypeTest extends PropSpec
with InterpreterLike 
with ParserLike
with HindleyMilnerLike
with ASTGenerators
with GeneratorDrivenPropertyChecks {
  
  def typeCheck(l:String) = {
    parseAll(LINE,l) match {
      case Success(ast,_) => newTyper(ast)
      case pr => fail("Term failed to parse"+pr)
    }
  }
  
  property("Identity has identical input and output types"){
    forAll {
      (id:String) =>
      typeCheck(s"$id . $id") shouldBe TFunc(TVar("a"),TVar("a"))
    }
  }  
  
  property("Constant function has different input and output types"){
    forAll {
      (a:String,b:String) =>
        whenever(a != b){
          typeCheck(s"$a . $b") shouldBe TFunc(TVar("a"),TVar("b"))
        }
    }
  }

  
}
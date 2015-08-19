/**
 * @author Felix
 */
import org.scalacheck.Gen
import org.scalacheck.Arbitrary._
import org.scalacheck.Arbitrary
import org.scalacheck.Prop._
import org.scalacheck.Properties
import it.vigtig.lambda._
import org.scalatest.PropSpec
import org.scalatest.prop.PropertyChecks
import org.scalatest.prop.Checkers
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.Matchers._

class FraseGenerator extends PropSpec
with InterpreterLike 
with Parser
with GeneratorDrivenPropertyChecks
 {
  import it.vigtig.lambda.LambdaAST._
  
  private object Generators {
    implicit def id = Arbitrary{ Gen.alphaChar map ( x => Id(""+x) ) }
    val int:Gen[Atom] = arbitrary[Int] map Integer  
    val float:Gen[Atom] = arbitrary[Float] map Floating
    
    implicit def bool:Arbitrary[Atom] = Arbitrary{ arbitrary[Boolean] map Bit }
    
    implicit def atom = Arbitrary{ Gen.oneOf(id,int,float,bool) }
  }
  
  import Generators._
  
  property("Ids are leaves"){
    forAll{
      (id:Id) => 
        size(id) shouldEqual 1
    }
  }

  property("Atoms cannot be reduced"){
    forAll {
     (a:Atom) => 
       interpret(a)() shouldEqual a
    }
  }
  
  property("Constant-function reduces to constant-function"){
    forAll{
      (x:Id,y:Id) => 
        whenever(x!=y){
          val str = s"((${x.id} . ${y.id}) ${x.id})"
          parseAll(LINE,str) match {
            case Success(ast, _) => 
              interpret(ast)() shouldBe y
            case _ => fail("Term failed to parse")
          }
        }
    }
  }
  
  property("'false' returns 2nd argument"){
    forAll{
      (x:Id,y:Id) => 
        val str = s"false ${x.id} ${y.id}"
        parseAll(LINE,str) match {
            case Success(ast, _) => 
              interpret(ast)() shouldBe y
            case _ => fail("Term failed to parse")
        }
    }
  }
  
    
  property("'true' returns 1st argument"){
    forAll{
      (x:Id,y:Id) => 
        val str = s"true ${x.id} ${y.id}"
        parseAll(LINE,str) match {
            case Success(ast, _) => 
              interpret(ast)() shouldBe x
            case _ => fail("Term failed to parse")
        }
    }
  }
  

}
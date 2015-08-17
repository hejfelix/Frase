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
       size(reduce(a)) shouldEqual size(a)
    }
  }
  
  

}
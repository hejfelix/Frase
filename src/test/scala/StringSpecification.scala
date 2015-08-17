/**
 * @author Felix
 */
import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import org.scalacheck.Gen
import it.vigtig.lambda.Parser
import it.vigtig.lambda.InterpreterLike
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Arbitrary

object StringSpecification extends Properties("String") 
with Parser 
with InterpreterLike{
  import it.vigtig.lambda.LambdaAST._
  
  private object Generators {
    val id:Gen[Atom] = Gen.alphaChar map ( x => Id(""+x) )
    val int:Gen[Atom] = arbitrary[Int] map Integer  
    val float:Gen[Atom] = arbitrary[Float] map Floating
    
    implicit def bool:Arbitrary[Atom] = Arbitrary{ arbitrary[Boolean] map Bit }
    
    implicit def atom = Arbitrary{ Gen.oneOf(id,int,float,bool) }
  }
  
  import Generators._
  
  property("canInterpretAtoms") = forAll {
   (a:Atom) => 
     size(reduce(a)) <= size(a)
  }
  
  property("startsWith") = forAll { (a: String, b: String) =>
    (a+b).startsWith(a)
  }


  property("substring") = forAll { (a: String, b: String, c: String) =>
    (a+b+c).substring(a.length, a.length+b.length) == b
  }

}
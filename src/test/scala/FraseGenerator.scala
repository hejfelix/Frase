/**
 * @author Felix
 */
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbBool
import org.scalacheck.Arbitrary.arbFloat
import org.scalacheck.Arbitrary.arbInt
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalatest.Matchers.convertToAnyShouldWrapper
import org.scalatest.PropSpec
import org.scalatest.prop.GeneratorDrivenPropertyChecks

import it.vigtig.lambda.ASTLike
import it.vigtig.lambda.InterpreterLike
import it.vigtig.lambda.ParserLike

trait ASTGenerators  {
  self:ASTLike =>
  implicit def id = Arbitrary { Gen.alphaChar map (x => Id("" + x)) }
  val int: Gen[Atom] = arbitrary[Int] map Integer
  val float: Gen[Atom] = arbitrary[Float] map Floating

  implicit def bool: Arbitrary[Atom] = Arbitrary { arbitrary[Boolean] map Bit }

  implicit def atom = Arbitrary { Gen.oneOf(id, int, float, bool) }
  
  implicit lazy val idStr = 
    Arbitrary {
    for {
      x <- Gen.alphaLowerChar 
      y <- Gen.alphaStr if x.isLetter
    } yield {x + y}
  }
}

class FraseGenerator extends PropSpec
    with InterpreterLike
    with ParserLike
    with ASTLike
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
    forAll {
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
          parseLineTest(s"false $x $y") (_ shouldBe Id(y))
        }
    }
  }  
  
  property("'true' returns 1st argument") {
    forAll {
      (x: String, y: String) => 
        whenever(x!=y){
          parseLineTest(s"true $x $y") (_ shouldBe Id(x))
        }
    }
  }


}
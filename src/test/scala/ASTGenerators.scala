

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen

import it.vigtig.lambda.AST
/*
 * @author Hargreaves
 */
trait ASTGenerators  {
  import AST._
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
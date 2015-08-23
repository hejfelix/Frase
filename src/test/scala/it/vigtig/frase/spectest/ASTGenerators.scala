

package it.vigtig.frase.spectest

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen

import it.vigtig.lambda.AST.Atom
import it.vigtig.lambda.AST.Bit
import it.vigtig.lambda.AST.Floating
import it.vigtig.lambda.AST.Id
import it.vigtig.lambda.AST.Integer
/*
 * @author Hargreaves
 */
trait ASTGenerators  {
  implicit def id = Arbitrary { Gen.alphaChar map (x => Id("" + x)) }
  val int: Gen[Atom] = arbitrary[Int] map Integer
  val float: Gen[Atom] = arbitrary[Float] map Floating

  implicit def bool: Arbitrary[Atom] = Arbitrary { arbitrary[Boolean] map Bit }

  implicit def atom = Arbitrary { Gen.oneOf(id, int, float, bool) }
  
  val keywords = Set("or","if")
  implicit lazy val idStr = 
    Arbitrary {
    for {
      x <- Gen.alphaLowerChar 
      y <- Gen.alphaStr if x.isLetter && !(x+y).startsWith("or")
    } yield {x.toString + y}
  }
}
package it.vigtig.frase.spectest

import it.vigtig.lambda.{HindleyMilnerLike, InterpreterLike, ParserLike}
import org.scalatest.Matchers.convertToAnyShouldWrapper
import org.scalatest.PropSpec
import org.scalatest.prop.GeneratorDrivenPropertyChecks

/*
 * @author Hargreaves
 */
class TypeTest extends PropSpec
with InterpreterLike
with ParserLike
with HindleyMilnerLike
with ASTGenerators
with GeneratorDrivenPropertyChecks {

  def typeCheck(l: String) = {
    parseAll(LINE, l) match {
      case Success(ast, _) => newTyper(ast)
      case pr => fail("Term failed to parse" + pr)
    }
  }

  property("Identity has identical input and output types") {
    forAll {
      (id: String) =>
        typeCheck(s"$id . $id") shouldBe TFunc(TVar("a"), TVar("a"))
    }
  }

  property("Constant function has different input and output types") {
    forAll {
      (a: String, b: String) =>
        whenever(a != b) {
          typeCheck(s"$a . $b") shouldBe TFunc(TVar("a"), TVar("b"))
        }
    }
  }

  property("TInst < TVar") {
    min(TInst("foo"), TVar("bar")).shouldBe(TInst("foo"))
  }

  property("Known types") {
    typeCheck("42") shouldBe TInst("Int")
    typeCheck("true") shouldBe TInst("Bool")
    typeCheck(".2f") shouldBe TInst("Float")
  }

}
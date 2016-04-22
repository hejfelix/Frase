package it.vigtig.frase.spectest.unit

import it.vigtig.lambda.HindleyMilnerLike
import org.scalatest.{FlatSpec, Matchers}

class HindleyMilnerLikeSpec
  extends FlatSpec
  with Matchers
  with HindleyMilnerLike {

  behavior of "Hindley-Milner type checker"

  it should "not unify type instances with different names" in {
    unify(TInst("a"),TInst("b"),Map.empty) shouldBe (TFail("TInst(a) != TInst(b)"),Map.empty)
  }

  it should "not unify polytypes with different names" in {
    val a = TPolyInst("A")
    val b = TPolyInst("B")
    unify(a,b,Map.empty) shouldBe (TFail(s"name mismatch: ${a.name} != ${b.name}"),Map.empty)
  }

  it should "not unify polytypes with different arities" in {
    val a = TPolyInst("A",TInst("x"))
    val b = TPolyInst("A")
    unify(a,b,Map.empty) shouldBe (TFail(s"arg length mismatch: ${a.args.length} != ${b.args.length}"),Map.empty)
  }

  it should "unify polytypes with same names and same arities" in {

    val a = TPolyInst("A",TInst("x"))
    val b = TPolyInst("A",TVar("y"))

    val EMPTY = Map.empty//Name it uppercase for pattern matching

    unify(a,b,Map.empty) should matchPattern {
      case (TPolyInst("A",TInst("x")),EMPTY) =>
    }
  }

}

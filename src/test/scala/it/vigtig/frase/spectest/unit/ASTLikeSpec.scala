package it.vigtig.frase.spectest.unit

import it.vigtig.lambda.ASTLike
import org.scalatest.{FlatSpec, Matchers}

class ASTLikeSpec  extends FlatSpec
  with
  Matchers
  with
  ASTLike {

  behavior of "pretty printing"

  it should "pretty print list terms" in {
    prettyList(Applic(Applic(SetId("Cons"),Integer(42)),Id("x"))) shouldBe List("42|x")
    prettyList(Applic(Applic(SetId("Cons"),Integer(42)),Integer(1337))) shouldBe List("42","1337")
    prettyList(SetId("Nil")) shouldBe Nil
    prettyList(Abstr(Id("x"),Id("y"))) shouldBe List("x . y")
  }

  it should "pretty print arbitrary terms" in {
    prettyStr(Applic(Applic(SetId("Cons"),Integer(1)),Integer(2))) shouldBe "[1,2]"
    prettyStr(SetId("Nil")) shouldBe ""
    prettyStr(ConstructorDef(Id("x"),List("a" -> "b"))) shouldBe "x (a,b)"
  }


}

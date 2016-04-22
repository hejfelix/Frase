package it.vigtig.frase.spectest.unit

import it.vigtig.lambda.InterpreterLike
import org.scalatest.{FlatSpec, Matchers}

class InterpreterLikeSpec
  extends FlatSpec
  with Matchers
  with InterpreterLike {

  behavior of "Interpreter"

  it should "succeed without named terms in program" in {
    interpretProgram("+ 1 2") shouldBe defined
  }


  it should "succeed without unnamed terms in program" in {
    interpretProgram("name = + 1 2") shouldBe defined
  }

  it should "fail on syntax errors" in {
    interpretProgram("..") shouldBe None
  }

  it should "interpret builtins even without typing" in {
    interpretProgram("== 1 1") shouldBe Some(List())
    interpretProgram("+ .2f .3f") shouldBe defined
    interpretProgram("% 4 3") shouldBe defined
  }

  it should "reduce named terms and lambda abstractions" in {
    reducer(Named(Id("x"),Applic(Applic(Id("+"),Integer(1)),Integer(2)))) shouldBe Named(Id("x"),Integer(3))
    reducer(Abstr(Id("x"),Applic(Applic(Id("+"),Integer(1)),Integer(2)))) shouldBe Abstr(Id("x"),Integer(3))
    reducer(Applic(Applic(Id("=="),Integer(1)),Integer(1))) shouldBe Bit(true)
    reducer(Applic(Applic(Id("+"),Floating(2f)),Floating(8f))) shouldBe (Floating(2f+8f))
  }

  it should "deal correctly with scope" in {
    interpretProgram("(x . y . x . x) 1 2 3") shouldBe Some(List(Integer(3)))
  }

  it should "transform correctly" in {
    transform({case Empty => Empty})(Applic(Integer(42),Integer(1337))) shouldBe (Applic(Integer(42),Integer(1337)))
    transform({case Empty => Empty})(Named(Id("x"),Id("y"))) shouldBe (Named(Id("x"),Id("y")))
  }


}

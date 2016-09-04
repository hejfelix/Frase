package it.vigtig.frase.spectest.unit

import it.vigtig.lambda.HindleyMilnerLike
import org.scalatest.{FlatSpec, Matchers}

class HindleyMilnerLikeSpec extends FlatSpec with Matchers with HindleyMilnerLike {

  behavior of "The unification algorithm"

  val EMPTY = Map.empty

  it should "not unify type instances with different names" in {
    unify(TInst("a"), TInst("b"), Map.empty) shouldBe (TFail("TInst(a) != TInst(b)"), Map.empty, Map.empty)
  }

  it should "not unify polytypes with different names" in {
    val a = TPolyInst("A")
    val b = TPolyInst("B")
    unify(a, b, Map.empty) shouldBe (TFail(s"name mismatch: ${a.name} != ${b.name}"), Map.empty, Map.empty)
  }

//  it should "not unify polytypes with different arities" in
//  {
//    val a = TPolyInst("A", TInst("x"))
//    val b = TPolyInst("A")
//    unify(a, b, Map.empty) shouldBe(TFail(s"arg length mismatch: ${a.args.length} != ${b.args.length}"), Map.empty)
//  }

  it should "not unify monotype instance with polytype instance" in {
    val a = TInst("Int")
    val b = TPolyInst("A")
    unify(a, b, Map.empty) shouldBe (TFail(s"$a could not unify with $b"), Map.empty, Map.empty)
  }

  it should "unify polytypes with same names" in {

    val a = TPolyInst("A", TInst("x"))
    val b = TPolyInst("A", TVar("y"))

    unify(a, b, Map.empty) should matchPattern {
      case (TPolyInst("A", TInst("x"), TNothing), EMPTY, _) =>
    }
  }

  behavior of "Hindley-Milner algorithm"

  it should "know types of builtin functions" in {

    val a = TVar("a")

    val binaryPattern: PartialFunction[Any, Unit] = {
      case ((TPolyInst(FUNC, `a`, TPolyInst(FUNC, `a`, `a`)), "b", EMPTY)) =>
    }

    w2(Id("+"), Map.empty, "a") should matchPattern(binaryPattern)

    w2(Id("*"), Map.empty, "a") should matchPattern(binaryPattern)

    w2(Id("-"), Map.empty, "a") should matchPattern(binaryPattern)

  }

  it should "infer types of branching functions" in {
    val branch: Term = Applic(Applic(Bit(true), Integer(42)), Integer(100))

    w2(branch, Map.empty, "a") should matchPattern {
      case (TInst("Int"), "a", EMPTY) =>
    }

    val a = TVar("a")
    val b = TVar("b")
    w2(Id("<="), Map.empty, "a") should matchPattern {
      case (TPolyInst(FUNC, `a`, TPolyInst(FUNC, `a`, TPolyInst(FUNC, `b`, TPolyInst(FUNC, `b`, `b`)))), "c", EMPTY) =>
    }
  }

  it should "infer type of named body" in {
    w2(Named(Id("hej"), Integer(42)), Map.empty, "a") should matchPattern {
      case (TInst("Int"), "a", EMPTY) =>
    }
  }

  it should "fail to infer types on ill-formed terms" in {
    w2(Applic(Integer(1), Integer(2)), Map.empty, "a") should matchPattern {
      case (TFail(_), _, EMPTY) =>
    }
  }

  it should "fail to infer type of abstractions with failed subterms" in {
    w2(Abstr(Id("x"), Applic(Integer(1), Integer(2))), Map.empty, "a") shouldBe (TFail("Type check failed"), "b", EMPTY)
  }

  it should "fail on undefined input" in {
    w2(ConstructorDef(Id("x"), Nil), Map.empty, "a") shouldBe (TFail(""), "a", EMPTY)
  }

  behavior of "type instantiator"

  it should "instantiate variables correctly" in {
    inst(TInst("x"), "a") shouldBe (TInst("x"), "a")
    inst(TVar("x"), "a") shouldBe (TVar("a"), "b")
    inst(TPolyInst("F", TVar("a"), TVar("b")), "a") shouldBe (TPolyInst("F", TVar("a"), TVar("b")), "c")
  }

  "newTyper" should "be just another typer" in {
    val typer = newTyper("a")
    typer(Named(Id("hej"), Integer(42))) should matchPattern {
      case (TInst("Int"), "a", EMPTY) =>
    }
  }

  behavior of "prettyType"

  it should "pretty print polytypes" in {
    prettyType(TPolyInst(FUNC, TVar("a"), TVar("b"))) shouldBe "a -> b"
    prettyType(TPolyInst("D", TVar("a"), TVar("b"))) shouldBe "D a -> b"
    prettyType(TFunc(TVar("a"), TVar("b"))) shouldBe "a -> b"
  }

  it should "print type variables and type instances" in {
    prettyType(TVar("x")) shouldBe "x"
    prettyType(TInst("Int")) shouldBe "Int"
  }

  it should "handle undefined input" in {
    prettyType(TUndefined) shouldBe "TUndefined"
  }

}

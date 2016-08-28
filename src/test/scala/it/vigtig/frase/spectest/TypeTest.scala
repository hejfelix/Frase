package it.vigtig.frase.spectest

import it.vigtig.lambda.{HindleyMilnerLike, InterpreterLike, ParserLike}
import org.scalatest.Matchers.convertToAnyShouldWrapper
import org.scalatest.PropSpec
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class TypeTest
    extends PropSpec
    with InterpreterLike
    with ParserLike
    with HindleyMilnerLike
    with ASTGenerators
    with GeneratorDrivenPropertyChecks {

  private val INTType: TInst = TInst("Int")

  def typeCheck(l: String): Type = {
    parseAll(LINE, l) match {
      case Success(ast, _) =>
        val (tpe, _, _) = w2(ast, Map(), "a")
        tpe
      case pr => fail("Term failed to parse" + pr)
    }
  }

  private val aVar: TVar = TVar("a")
  private val bVar: TVar = TVar("b")

  property("Identity has identical input and output types") {
    typeCheck("x . x") shouldBe TPolyInst(FUNC, aVar, aVar)
  }

  property("Constant function has different input and output types") {
    typeCheck("x . y") shouldBe TPolyInst(FUNC, aVar, bVar)
  }

  property("Abstraction w. addition should yield specific type") {
    typeCheck("x . + x 2") shouldBe TPolyInst(FUNC, INTType, INTType)
  }

  property("Known types") {
    typeCheck("42") shouldBe INTType
    typeCheck("true") shouldBe TPolyInst(FUNC, aVar, TPolyInst(FUNC, aVar, aVar))
    typeCheck(".2f") shouldBe TInst("Float")
  }

  property("Free Type Variables") {

    freeVars(TPolyInst("dude", aVar, bVar, TInst("c"), TVar("d"))) shouldBe Set(aVar, bVar, TVar("d"))

    val ctx: Map[Term, Type] = Map(
        Id("a") -> TInst("sometype"),
        Id("b") -> TVar("someOtherType"),
        Id("c") -> TPolyInst("dude", aVar, bVar, TInst("c"), TVar("d"))
    )

    val expected = Set(aVar, bVar, TVar("d"), TVar("someOtherType"))

    freeVars(ctx) shouldBe expected

  }

  property("Specialization") {

    val tpe1 = TPolyInst("Dude", aVar, TInst("int"))
    val tpe2 = TPolyInst("Dude", TInst("float"), TInst("int"))
    val tpe3 = TPolyInst("Dude", bVar, TVar("c"))

    val singletonType = TPolyInst("Thing")

    assert(moreGeneralOrEqual(tpe1, tpe2, Map()))
    assert(moreGeneralOrEqual(tpe3, tpe1, Map()))
    assert(moreGeneralOrEqual(tpe3, tpe2, Map()))

    assert(moreGeneralOrEqual(tpe1, tpe1, Map()))
    assert(moreGeneralOrEqual(tpe2, tpe2, Map()))
    assert(moreGeneralOrEqual(tpe3, tpe3, Map()))

    assert(moreGeneralOrEqual(singletonType, singletonType, Map()))

  }

  def unifyType(tpe1: Type, tpe2: Type): Type = unify(tpe1, tpe2, Map())._1

  property("Unification") {

    val tpe1 = TPolyInst("Dude", aVar, INTType)
    val tpe2 = TPolyInst("Dude", TInst("float"), INTType)
    val tpe3 = TPolyInst("Dude", bVar, TVar("c"))

    val tpe4 = TPolyInst(FUNC, aVar, aVar)
    val tpe5 = TPolyInst(FUNC, INTType, aVar)

    val singletonType = TPolyInst("Thing")

    unifyType(tpe1, tpe2) shouldBe TPolyInst("Dude", TInst("float"), INTType)
    unifyType(tpe1, tpe1) shouldBe tpe1

    unifyType(tpe1, tpe3) shouldBe tpe1

    unifyType(singletonType, singletonType) shouldBe singletonType

    unifyType(tpe4, tpe5) shouldBe TPolyInst(FUNC, INTType, INTType)
  }

  property("W2 test") {

    val i: Int   = 42
    val f: Float = 42f
    val integer  = Integer(i)
    val float    = Floating(f)
    val bit      = Bit(true)

    w2(integer, Map(), "a") shouldBe (INTType, "a", Map())
    w2(float, Map(), "a") shouldBe (TInst("Float"), "a", Map())
    w2(bit, Map(), "a") shouldBe ((TPolyInst(FUNC, aVar, TPolyInst(FUNC, aVar, aVar)), "b", Map()))

    w2(Id("x"), Map(), "a") shouldBe (aVar, "b", Map(Id("x") -> aVar))

    w2(Abstr(Id("x"), Id("y")), Map(), "a") match {
      case (tpe, next, _) =>
        tpe shouldBe TPolyInst(FUNC, aVar, bVar)
        next shouldBe "c"
    }

    val application = Applic(Abstr(Id("x"), Id("x")), Integer(i))

    val identity = Abstr(Id("x"), Id("x"))

    w2(identity, Map(), "a") match {
      case (tpe, next, _) =>
        tpe shouldBe TPolyInst(FUNC, aVar, aVar)
        next shouldBe "b"
    }

    w2(application, Map(), "a") match {
      case (tpe, next, _) =>
        tpe shouldBe INTType
        next shouldBe "c"
    }

  }

}

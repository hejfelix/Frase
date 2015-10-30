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
    typeCheck("x . x") shouldBe TPolyInst("Func",TVar("a"), TVar("a"))

  }

  property("Constant function has different input and output types") {
       typeCheck("x . y") shouldBe TPolyInst("Func",TVar("a"), TVar("b"))
  }


  property("Known types") {
    typeCheck("42") shouldBe TInst("Int")
    typeCheck("true") shouldBe TInst("Bool")
    typeCheck(".2f") shouldBe TInst("Float")
  }

  property("Free Type Variables") {


    freeVars(TPolyInst("dude",TVar("a"),TVar("b"),TInst("c"),TVar("d"))) shouldBe Set(TVar("a"),TVar("b"),TVar("d"))

    val ctx: Map[Term, Type] = Map(
      Id("a") -> TInst("sometype"),
      Id("b") -> TVar("someOtherType"),
      Id("c") -> TPolyInst("dude",TVar("a"),TVar("b"),TInst("c"),TVar("d"))
    )

    val expected = Set(TVar("a"),TVar("b"),TVar("d"),TVar("someOtherType"))

    freeVars(ctx) shouldBe expected

  }


  property("Specialization") {

    val tpe1 = TPolyInst("Dude",TVar("a"),TInst("int"))
    val tpe2 = TPolyInst("Dude",TInst("float"),TInst("int"))
    val tpe3 = TPolyInst("Dude",TVar("b"),TVar("c"))

    val singletonType = TPolyInst("Thing")

    assert(moreGeneralOrEqual(tpe1,tpe2,Map()))
    assert(moreGeneralOrEqual(tpe3,tpe1,Map()))
    assert(moreGeneralOrEqual(tpe3,tpe2,Map()))


    assert(moreGeneralOrEqual(tpe1,tpe1,Map()))
    assert(moreGeneralOrEqual(tpe2,tpe2,Map()))
    assert(moreGeneralOrEqual(tpe3,tpe3,Map()))

    assert(moreGeneralOrEqual(singletonType,singletonType,Map()))

  }


  property("Unification") {

    val tpe1 = TPolyInst("Dude",TVar("a"),TInst("int"))
    val tpe2 = TPolyInst("Dude",TInst("float"),TInst("int"))
    val tpe3 = TPolyInst("Dude",TVar("b"),TVar("c"))

    val tpe4 = TPolyInst("Func",TVar("a"),TVar("a"))
    val tpe5 = TPolyInst("Func",TInst("Int"),TVar("a"))

    val singletonType = TPolyInst("Thing")

    unify(tpe1,tpe2) shouldBe TPolyInst("Dude",TInst("float"),TInst("int"))
    unify(tpe1,tpe1) shouldBe tpe1
    unify(tpe1,tpe3) shouldBe tpe1

    unify(singletonType,singletonType) shouldBe singletonType

    unify(tpe4,tpe5) shouldBe TPolyInst("Func",TInst("Int"),TInst("Int"))
  }

  property("W2 test") {

    def newVariableGenerator = {
      var nextVar = 'a'
      () => {
        val v = nextVar
        nextVar = (nextVar + 1).toChar
        ""+v
      }
    }

    val integer = Integer(42)
    val float = Floating(42f)
    val bit = Bit(true)

    w2(integer,Map(),newVariableGenerator) shouldBe TInst("Int")
    w2(float,Map(),newVariableGenerator) shouldBe TInst("Float")
    w2(bit,Map(),newVariableGenerator) shouldBe TInst("Bool")

    w2(Id("x"),Map(),newVariableGenerator) shouldBe TVar("a")

    w2(Abstr(Id("x"),Id("y")),Map(),newVariableGenerator) shouldBe TPolyInst("Func",TVar("a"),TVar("b"))

    val application = Applic(Abstr(Id("x"),Id("x")),Integer(42))

    val identity = Abstr(Id("x"),Id("x"))

    w2(identity,Map(),newVariableGenerator) shouldBe TPolyInst("Func",TVar("a"),TVar("a"))

    w2(application,Map(),newVariableGenerator) shouldBe TInst("Int")

  }




}
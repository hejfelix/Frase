package com.lambdaminute.types

import com.lambdaminute.syntax.AST
import com.lambdaminute.syntax.AST.{Application, Identifier, LambdaAbstraction, Term}
import org.scalatest.{Matchers, WordSpec}

class TyperSpec extends WordSpec with Matchers {

  "Typer" should {
    val xid = Identifier("x")
    val yid = Identifier("y")

    def prettyPrintMap(typeMap: Map[Term, Type], tag: String): Unit = {
      println(tag)
      typeMap.foreach {
        case (term, tpe) => print(s"${term.pretty}: ${tpe.prettyType},    ")
      }
      println()
    }

    "expand nested terms" in {

      val topLevel = LambdaAbstraction(xid, Application(xid, yid))
      val appl     = Application(xid, yid)
      val variables: Map[Term, Type] = Map(
        xid      -> Identifier("a").asType,
        yid      -> Identifier("b").asType,
        appl     -> Identifier("c").asType,
        topLevel -> Identifier("d").asType
      )

      val typer = Typer()

      val (res, _) = typer.expandMap(variables)

      println(s"Typing expression: ${topLevel.pretty}")
      println()
      prettyPrintMap(variables, "Before expansion")
      prettyPrintMap(res, "Expanded")

      val expectedXidType = LambdaAbstraction(Identifier("b"), Identifier("a"))
      val expected = variables
        .updated(xid, expectedXidType.asType)
        .updated(appl, Identifier("a").asType)
        .updated(topLevel, LambdaAbstraction(expectedXidType, Identifier("a")).asType)

      res shouldBe expected
    }

    "find variables" in {

      val x    = xid
      val y    = yid
      val app  = Application(x, y)
      val term = LambdaAbstraction(x, app)

      val typer = Typer()

      val xType    = x    -> typer.Var("a").asTypeId
      val yType    = y    -> typer.Var("b").asTypeId
      val appType  = app  -> typer.Var("c").asTypeId
      val termType = term -> typer.Var("d").asTypeId

      val expected: Map[AST.Term, Type] = Map(xType, yType, termType, appType)

      val (result, _) = typer.variables(term, Map.empty, typer.Var("a"))

      prettyPrintMap(result, "found variables:")

      result shouldBe expected

    }

    "expand variables in `lambda-abstraction`s" in {

      val aid = Identifier("a")
      val bid = Identifier("b")
      val cid = Identifier("c")

      val abstr = LambdaAbstraction(xid, yid)

      val variableTypes: Map[AST.Term, Type] =
        Map(xid -> aid.asType, yid -> bid.asType, abstr -> cid.asType)

      prettyPrintMap(variableTypes, "Expanding variables")
      val typer   = Typer()
      val nextVar = typer.Var("d")

      val (expandedResult, _) = typer.expandMap(variableTypes, nextVar)

      prettyPrintMap(expandedResult, "Resulting expansion:")
      val expectedAbstrType: Type = LambdaAbstraction(aid, bid).asType
      val expectedResult: Map[AST.Term, Type] =
        variableTypes.updated(abstr, expectedAbstrType)

      expandedResult shouldBe expectedResult
    }

    "expand variables in `application`s" in {

      val aid = Identifier("a")
      val bid = Identifier("b")
      val cid = Identifier("c")

      val appl = Application(xid, yid)

      val variableTypes: Map[AST.Term, Type] =
        Map(xid -> aid.asType, yid -> bid.asType, appl -> cid.asType)

      val typer   = Typer()
      val nextVar = typer.Var("d")

      prettyPrintMap(variableTypes, "Expanding variables")

      val (expandedResult, _) = typer.expandMap(variableTypes, nextVar)

      prettyPrintMap(expandedResult, "Resulting expansion:")

      val expectedXType: Type = LambdaAbstraction(variableTypes(yid).asTerm, Identifier(nextVar.id)).asType
      val expectedResult: Map[AST.Term, Type] =
        variableTypes.updated(xid, expectedXType).updated(appl, nextVar.asTypeId)

      expandedResult shouldBe expectedResult
    }
  }

}

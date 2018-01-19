package com.lambdaminute.types

import com.lambdaminute.interactive.ParserHelper
import com.lambdaminute.syntax.AST
import com.lambdaminute.syntax.AST.{Application, Identifier, LambdaAbstraction, Term}
import org.scalatest.{Matchers, WordSpec}

class TyperSpec extends WordSpec with Matchers with ParserHelper {

  "Typer" should {
    val xid = Identifier("x")
    val yid = Identifier("y")

    def prettyPrintMap(typeMap: Map[Term, Type], tag: String): Unit = {
      println(tag)
      typeMap.toList.sortBy { case (term, _) => term.size }.foreach {
        case (term, tpe) => print(s"${term.pretty}: ${tpe.prettyType},    ")
      }
      println()
    }

    "deal with capturing3" in {
      val typer      = Typer(new UnificationLike(), logging = true)
      val termString = "((x . x . x) 2) 3"
      val term       = termString.toTerm
      println(termString)
      println()

      val (context, _) = typer.infer()(term)
      prettyPrintMap(context, "inferred once")

      context(term) shouldBe Identifier("Int").asType
    }

    "applications should infer abstraction in left argument" in {
      val term         = "x y".toTerm
      val typer        = Typer(new UnificationLike())
      val (context, _) = typer.infer()(term)
      prettyPrintMap(context, "type of application")

      context("x".toTerm) shouldBe "a . b".toType
      context(term) shouldBe "b".toType
    }

    "abstraction should infer based on body and argument" in {
      val term         = "x . 42".toType
      val typer        = Typer(new UnificationLike())
      val (context, _) = typer.infer()(term)
      prettyPrintMap(context, "type of abstraction")

      context("42".toTerm) shouldBe Identifier("Int").asType
    }

    "expand variables in `application`s" in {

      val aid = Identifier("a")
      val bid = Identifier("b")
      val cid = Identifier("c")

      val appl = Application(xid, yid)

      val variableTypes: Map[AST.Term, Type] =
        Map(xid -> aid.asType, yid -> bid.asType, appl -> cid.asType)

      val typer   = Typer(new UnificationLike())
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

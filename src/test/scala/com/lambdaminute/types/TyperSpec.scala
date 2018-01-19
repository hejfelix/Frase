package com.lambdaminute.types

import com.lambdaminute.interactive.ParserHelper
import com.lambdaminute.syntax.AST.{Identifier, Term}
import org.scalatest.{Matchers, WordSpec}

class TyperSpec extends WordSpec with Matchers with ParserHelper {

  "Typer" should {

    def prettyPrintMap(typeMap: Map[Term, Type], tag: String): Unit = {
      println(tag)
      typeMap.toList.sortBy { case (term, _) => term.size }.foreach {
        case (term, tpe) => print(s"${term.pretty}: ${tpe.prettyType},    ")
      }
      println()
    }

    "deal with capturing3" in {
      val typer      = Typer(new UnificationLike(logging = true), logging = true)
      val termString = "((x . x . x) 2) 3"
      val term       = termString.toTerm
      println(termString)
      println()

      val (context, _) = typer.infer()(term)
      prettyPrintMap(context, "inferred")

      context(term) shouldBe Identifier("Int").asType
    }

    "deal with capturing4" in {
      val typer      = Typer(new UnificationLike(logging = true), logging = true)
      val termString = "((x . x . x) 2) y"
      val term       = termString.toTerm
      println(termString)
      println()

      val (context, _) = typer.infer()(term)
      prettyPrintMap(context, "inferred")

      context(term) shouldBe Identifier("b").asType
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

  }

}

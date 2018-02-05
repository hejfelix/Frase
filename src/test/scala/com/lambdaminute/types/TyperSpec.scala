package com.lambdaminute.types

import com.lambdaminute.interactive.ParserHelper
import com.lambdaminute.ast.AST.{Identifier, LambdaAbstraction, Term}
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
      val typer      = Typer(new DefaultUnification(logging = true), logging = true)
      val termString = "((x . x . x) 2) 3"
      val term       = termString.toTerm
      println(termString)
      println()

      val (context, _) = typer.infer()(term)
      prettyPrintMap(context, "inferred")

      context(term) shouldBe Identifier("Int").asType
    }

    "deal with capturing4" in {
      val typer      = Typer(new DefaultUnification(logging = true), logging = true)
      val termString = "((x . x . x) 2) y"
      val term       = termString.toTerm
      println(termString)
      println()

      val (context, _) = typer.infer()(term)
      prettyPrintMap(context, "inferred")

      context(term) shouldBe Identifier("f").asType
    }

    "applications should infer abstraction in left argument" in {
      val term         = "x y".toTerm
      val typer        = Typer(new DefaultUnification())
      val (context, _) = typer.infer()(term)
      prettyPrintMap(context, "type of application")

      context("x".toTerm) shouldBe "a . c".toType
      context(term) shouldBe "c".toType
    }

    "nested abstractions should infer correctly" in {
      val programText  = "(x . 42) z"
      val term         = programText.toTerm
      val typer        = Typer(new DefaultUnification(logging = true), logging = true)
      val (context, _) = typer.infer()(term)
      println(programText)
      prettyPrintMap(context, "type of nested abstraction")

      context("x . 42".toTerm) shouldBe LambdaAbstraction(Identifier("a"), Identifier("Int")).asType
    }

    "abstraction should infer based on body and argument" in {
      val term         = "x . 42".toType
      val typer        = Typer(new DefaultUnification())
      val (context, _) = typer.infer()(term)
      prettyPrintMap(context, "type of abstraction")

      context("42".toTerm) shouldBe Identifier("Int").asType
    }

    "chain types in deep nesting" in {
      val programText  = "(x . y . z . zz . zzz . 42)"
      val term         = programText.toTerm
      val typer        = Typer(new DefaultUnification())
      val (context, _) = typer.infer()(term)
      prettyPrintMap(context, "type of nesting")

      context(term) shouldBe LambdaAbstraction(
        Identifier("e"),
        LambdaAbstraction(Identifier("d"),
                          LambdaAbstraction(Identifier("c"),
                                            LambdaAbstraction(Identifier("b"),
                                                              LambdaAbstraction(Identifier("a"), Identifier("Int")))))
      )
    }

    "deal with shadowing" in {
      val programText  = "(x . x . x . x . x . 42)"
      val term         = programText.toTerm
      val typer        = Typer(new DefaultUnification())
      val (context, _) = typer.infer()(term)
      prettyPrintMap(context, "shadowing")

      context(term) shouldBe LambdaAbstraction(
        Identifier("e"),
        LambdaAbstraction(Identifier("d"),
                          LambdaAbstraction(Identifier("c"),
                                            LambdaAbstraction(Identifier("b"),
                                                              LambdaAbstraction(Identifier("a"), Identifier("Int")))))
      )
    }

    "deal with built ins" in {
      val programText  = "true"
      val term         = programText.toTerm
      val typer        = Typer(new DefaultUnification())
      val (context, _) = typer.infer()(term)
      prettyPrintMap(context, "boolean")

      context(term) shouldBe "a . a . a".toType
    }

    "deal with built ins2" in {
      val programText  = "false"
      val term         = programText.toTerm
      val typer        = Typer(new DefaultUnification())
      val (context, _) = typer.infer()(term)
      prettyPrintMap(context, "boolean")

      context(term) shouldBe "a . a . a".toType
    }

    "deal with built ins3" in {
      val programText  = "false 42 x"
      val term         = programText.toTerm
      val typer        = Typer(new DefaultUnification())
      val (context, _) = typer.infer()(term)
      prettyPrintMap(context, "boolean")

      context(term) shouldBe Identifier("Int").asType
    }

    "deal with built ins4" in {
      val programText = "true 10 13.37"
      val term        = programText.toTerm
      val typer       = Typer(new DefaultUnification(true), logging = true)
      assertThrows[NoSuchElementException] {
        typer.infer()(term)
      }
    }

  }

}

package com.lambdaminute.types

import com.lambdaminute.interactive.ParserHelper
import com.lambdaminute.syntax.AST.{Identifier, Term}
import org.scalatest.{Matchers, WordSpec}
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class TermSpec extends WordSpec with GeneratorDrivenPropertyChecks with TermGenerators with Matchers with ParserHelper {

  "Terms" should {

    "unify with themselves" in {
      forAll { a: Term =>
        a.unify(a) should matchPattern {
          case Right(_) =>
        }
      }
    }

    "always unify single variables" in {
      forAll { (term: Term, id: Identifier) =>
        id.unify(term) shouldBe Right(Map(id -> term))
      }
    }

    "unify applications" in {
      "a b".toTerm.unify("x y".toTerm) shouldBe Right(
        Map(
          "a".toTerm -> "x".toTerm,
          "b".toTerm -> "y".toTerm
        ))
    }

    "unify all variables if terms have same shape" in {
      val a = "x . y . (x y)".toTerm
      val b = "a . b . (a b)".toTerm
      a.unify(b) shouldBe Right(
        Map(
          "x".toTerm -> "a".toTerm,
          "y".toTerm -> "b".toTerm
        ))
    }

    "fail to unify if contradicting substitutions occur" in {
      val a      = "x . y . x".toTerm
      val b      = "1 . b . 2".toTerm
      val result = a.unify(b)
      println(result)
      result should matchPattern {
        case Left("Unable to unify 1 with 2") =>
      }
    }

    "find unification for duplicate occurrence of variables" in {
      val a = "x . y . x".toTerm
      val b = "a . b . b".toTerm
      a.unify(b) should matchPattern {
        case Right(_) =>
      }
    }

    "unshadow captured variables" in {
      val exp        = "x . x . x".toTerm
      val unshadowed = exp.unshadow
      unshadowed shouldBe "y . x . x".toTerm
    }

    "unshadow captured variables2" in {
      val exp        = "x . x . x y".toTerm
      val unshadowed = exp.unshadow
      unshadowed shouldBe "z . x . x y".toTerm
    }
  }

}

package com.lambdaminute.frase.lang.types

import com.lambdaminute.frase.calculus.interactive.ParserHelper
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Matchers, WordSpec}

class TermSpec extends WordSpec with GeneratorDrivenPropertyChecks with TermGenerators with Matchers with ParserHelper {

  "Terms" should {

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

    "unshadow captured variables3" in {
      val exp        = "x . x . x . x y".toTerm
      val unshadowed = exp.unshadow
      unshadowed shouldBe "z . z . x . x y".toTerm
    }
  }

}

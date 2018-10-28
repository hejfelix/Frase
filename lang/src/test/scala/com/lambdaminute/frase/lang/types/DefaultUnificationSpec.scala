package com.lambdaminute.frase.lang.types

import com.lambdaminute.frase.calculus.interactive.ParserHelper
import com.lambdaminute.frase.calculus.ast.Ast._
import org.scalatest.{Matchers, WordSpec}

class DefaultUnificationSpec extends WordSpec with Matchers with ParserHelper {

  private val loggingEnabled = false

  def prettyEq(eq: (Term, Term)) =
    s"${eq._1.pretty} = ${eq._2.pretty}"

  "Unification" should {
    "do stuff" in {

      val unification = new DefaultUnification(logging = loggingEnabled)

      val t1 = "a . a".toTerm
      val t2 = LambdaAbstraction(Identifier("Int"), Identifier("b"))

      unification.unifyFix(List(t1 -> t2)).right.get
    }

    "fixpoint" in {
      val unification = new DefaultUnification(logging = loggingEnabled)
      unification.fixPoint(100)(_ / 2) shouldBe 0

      unification.fixPoint(List(1, 2, 3, 4))(_.drop(1)) shouldBe Nil
    }
  }

}

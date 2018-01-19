package com.lambdaminute.types

import com.lambdaminute.interactive.ParserHelper
import com.lambdaminute.syntax.AST._
import org.scalatest.{Matchers, WordSpec}

class UnificationLikeSpec extends WordSpec with Matchers with ParserHelper {

  def prettyEq(eq: (Term, Term)) =
    s"${eq._1.pretty} = ${eq._2.pretty}"

  "Unification" should {
    "do stuff" in {

      val unification = new UnificationLike(logging = true)

      val t1 = "a . a".toTerm
      val t2 = LambdaAbstraction(Identifier("Int"), Identifier("b"))

      val result: List[(Term, Term)] = unification.unifyFix(List(t1 -> t2)).right.get
      println(result.map(prettyEq).mkString(",   "))

    }

    "fixpoint" in {
      val unification = new UnificationLike()
      unification.fixPoint(100)(_ / 2) shouldBe 0

      unification.fixPoint(List(1, 2, 3, 4))(_.drop(1)) shouldBe Nil
    }
  }

}

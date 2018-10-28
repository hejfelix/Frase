package com.lambdaminute.frase.calculus.semantic

import com.lambdaminute.frase.calculus.ast.Ast.Identifier

trait Keywords {
  val yCombinatorKeyword: String

  val yCombinator: Identifier
}

case class DefaultKeywords() extends Keywords {

  val yCombinatorKeyword = "yCombinator"

  val yCombinator = Identifier("yCombinator")

}

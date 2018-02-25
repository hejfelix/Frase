package com.lambdaminute.frase.calculus.semantic

import com.lambdaminute.frase.calculus.ast.AST.Identifier

trait Keywords {
  val yCombinatorKeyword: String

  val yCombinator: Identifier
}

case class DefaultKeywords() extends Keywords {

  val yCombinatorKeyword = "yCombinator"

  val yCombinator = Identifier("yCombinator")

}

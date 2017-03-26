package it.vigtig.lambda.semantic

import it.vigtig.lambda.syntax.AST.Identifier

trait Keywords {
  val yCombinatorKeyword: String

  val yCombinator: Identifier
}

case class DefaultKeywords() extends Keywords {

  val yCombinatorKeyword = "yCombinator"

  val yCombinator = Identifier("yCombinator")

}

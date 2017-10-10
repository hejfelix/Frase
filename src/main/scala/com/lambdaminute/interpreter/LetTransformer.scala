package it.vigtig.lambda.interpreter

import it.vigtig.lambda.errors.FraseError
import it.vigtig.lambda.syntax.AST._

trait LetTransformer {

  def transform(fragments: List[Fragment]): Either[FraseError, Term]

}



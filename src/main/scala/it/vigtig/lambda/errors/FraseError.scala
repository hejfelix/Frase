package it.vigtig.lambda.errors

sealed trait FraseError {
  def msg: String
}

case class GenericError(msg: String) extends FraseError

case class LexerError(err: String, nextToken: String) extends FraseError {
  def msg = s"$err, next token: $nextToken"
}
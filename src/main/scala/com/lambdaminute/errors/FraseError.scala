package com.lambdaminute.errors

sealed trait FraseError {
  def msg: String
}

case class GenericError(msg: String) extends FraseError

case class ParsingError[T](err: String, next: T) extends FraseError {
  def msg = s"$err, next temr: ${next}"
}
case class LexerError[T](err: String, nextToken: T) extends FraseError {
  def msg = s"$err, next token: $nextToken"
}

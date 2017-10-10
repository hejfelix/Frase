package com.lambdaminute.syntax

import com.lambdaminute.errors.FraseError

trait Lexer {
  def tokenize(program: String): Either[FraseError, List[Token]]
}

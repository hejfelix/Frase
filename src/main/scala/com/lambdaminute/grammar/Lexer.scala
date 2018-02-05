package com.lambdaminute.grammar

import com.lambdaminute.errors.FraseError

trait Lexer {
  def tokenize(program: String): Either[FraseError, List[Token]]
}

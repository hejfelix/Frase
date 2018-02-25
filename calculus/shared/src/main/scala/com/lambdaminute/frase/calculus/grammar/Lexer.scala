package com.lambdaminute.frase.calculus.grammar

import com.lambdaminute.frase.calculus.errors.FraseError

trait Lexer {
  def tokenize(program: String): Either[FraseError, List[Token]]
}

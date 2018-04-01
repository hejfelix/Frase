package com.lambdaminute.frase.calculus

import com.lambdaminute.frase.calculus.grammar.{DefaultLexer, DefaultParser}
import com.lambdaminute.frase.calculus.interpreter.{BetaReduction, DefaultBuiltins, DefaultInterpreter, Interpreter}
import com.lambdaminute.frase.calculus.semantic.{DefaultKeywords, Keywords}

package object interactive extends ParserHelper {

  def defaultKeywords                            = DefaultKeywords()
  def lexer                                      = DefaultLexer()
  def parser                                     = DefaultParser(lexer)
  def defaultBuiltins: Keywords => BetaReduction = DefaultBuiltins.builtIns
  def interpreter: Interpreter =
    DefaultInterpreter(parser, defaultKeywords, defaultBuiltins)

}

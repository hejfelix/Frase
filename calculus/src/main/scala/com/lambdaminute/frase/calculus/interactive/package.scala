package com.lambdaminute.frase.calculus

import com.lambdaminute.frase.calculus.grammar.{DefaultLexer, DefaultParser}
import com.lambdaminute.frase.calculus.interpreter.{DefaultBuiltins, DefaultInterpreter, Interpreter}
import com.lambdaminute.frase.calculus.semantic.DefaultKeywords

package object interactive extends ParserHelper {

  def defaultKeywords = DefaultKeywords()
  def lexer           = DefaultLexer()
  def parser          = DefaultParser(lexer)
  def defaultBuiltins = DefaultBuiltins.builtIns
  def interpreter: Interpreter =
    DefaultInterpreter(parser, defaultKeywords, defaultBuiltins)

}

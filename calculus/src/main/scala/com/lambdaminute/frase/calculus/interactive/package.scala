package com.lambdaminute.frase.calculus

import com.lambdaminute.frase.calculus.interpreter.{DefaultBuiltins, DefaultInterpreter, DefaultLetTransformer, Interpreter}
import com.lambdaminute.frase.calculus.semantic.DefaultKeywords
import com.lambdaminute.frase.calculus.grammar.{DefaultLexer, DefaultParser}

package object interactive extends ParserHelper {

  def defaultKeywords          = DefaultKeywords()
  def lexer                    = DefaultLexer()
  def parser                   = DefaultParser(lexer)
  def letTransformer           = DefaultLetTransformer(defaultKeywords)
  def defaultBuiltins          = DefaultBuiltins.builtIns
  def interpreter: Interpreter = DefaultInterpreter(parser, letTransformer, defaultKeywords, defaultBuiltins)

}

package com.lambdaminute

import com.lambdaminute.interpreter.{DefaultBuiltins, DefaultInterpreter, DefaultLetTransformer, Interpreter}
import com.lambdaminute.semantic.DefaultKeywords
import com.lambdaminute.grammar.{DefaultLexer, DefaultParser}

package object interactive extends ParserHelper {

  def defaultKeywords          = DefaultKeywords()
  def lexer                    = DefaultLexer()
  def parser                   = DefaultParser(lexer)
  def letTransformer           = DefaultLetTransformer(defaultKeywords)
  def defaultBuiltins          = DefaultBuiltins.builtIns
  def interpreter: Interpreter = DefaultInterpreter(parser, letTransformer, defaultKeywords, defaultBuiltins)

}

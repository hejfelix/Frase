package com.lambdaminute

import com.lambdaminute.interpreter.{DefaultInterpreter, DefaultLetTransformer, Interpreter}
import com.lambdaminute.semantic.DefaultKeywords
import com.lambdaminute.grammar.{DefaultLexer, DefaultParser}

package object interactive extends ParserHelper {

  def defaultKeywords          = DefaultKeywords()
  def lexer                    = DefaultLexer()
  def parser                   = DefaultParser(lexer)
  def letTransformer           = DefaultLetTransformer(defaultKeywords)
  def interpreter: Interpreter = DefaultInterpreter(parser, letTransformer, defaultKeywords)

}

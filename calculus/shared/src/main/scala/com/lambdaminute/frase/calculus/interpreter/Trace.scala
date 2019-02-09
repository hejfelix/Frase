package com.lambdaminute.frase.calculus.interpreter
import com.lambdaminute.frase.calculus.ast.Ast.{Identifier, Term}

sealed trait Trace
case class ApplicationTrace(variable: Identifier, substitution: Term, in: Term) extends Trace
case class BuiltInTrace(term: Term, substitution: Term) extends Trace

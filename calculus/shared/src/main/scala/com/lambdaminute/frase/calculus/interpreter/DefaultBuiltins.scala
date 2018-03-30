package com.lambdaminute.frase.calculus.interpreter

import com.lambdaminute.frase.calculus.ast.AST
import com.lambdaminute.frase.calculus.ast.AST._
import com.lambdaminute.frase.calculus.semantic.Keywords

object DefaultBuiltins {
  private val App: AST.Application.type = Application
  def builtIns: Keywords => BetaReduction = keyWords => {
    import keyWords._
    {
      case (nextId, App(`yCombinator`, f)) => (nextId, App(f, App(yCombinator, f)))
      case (nextId, yCombExp @ LambdaAbstraction(`yCombinator`, body)) =>
        (nextId, App(body, yCombExp))
      case (nextId, App(App(Identifier("=="), a), b))                   => (nextId, Bool(a == b))
      case (nextId, App(App(Identifier("<="), Integer(a)), Integer(b))) => (nextId, Bool(a <= b))
      case (nextId, Bool(b)) =>
        val yes  = nextId
        val no   = yes.nextAvailableId
        val next = no.nextAvailableId
        (next, LambdaAbstraction(yes, LambdaAbstraction(no, if (b) yes else no)))
      case (nextId, App(App(Identifier("*"), Integer(x)), Integer(y))) => (nextId, Integer(x * y))
      case (nextId, App(App(Identifier("+"), Integer(x)), Integer(y))) => (nextId, Integer(x + y))
      case (nextId, App(App(Identifier("+"), Floating(x)), Floating(y))) =>
        (nextId, Floating(x + y))
      case (nextId, App(App(Identifier("-"), Integer(x)), Integer(y))) => (nextId, Integer(x - y))
      case (nextId, App(App(Identifier("%"), Integer(a)), Integer(b))) => (nextId, Integer(a % b))
    }
  }

}

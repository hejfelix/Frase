package com.lambdaminute.frase.calculus.interpreter

import com.lambdaminute.frase.calculus.ast.AST
import com.lambdaminute.frase.calculus.ast.AST._
import com.lambdaminute.frase.calculus.semantic.Keywords

object DefaultBuiltins {
  private val App: AST.Application.type = Application
  def builtIns: Keywords => PartialFunction[Term, Term] = keyWords => {
    import keyWords._
    {
      case App(`yCombinator`, f)                               => App(f, App(yCombinator, f))
      case yCombExp @ LambdaAbstraction(`yCombinator`, body)   => Application(body, yCombExp)
      case App(App(Identifier("=="), a), b)                    => Bool(a == b)
      case App(App(Identifier("<="), Integer(a)), Integer(b))  => Bool(a <= b)
      case App(App(Bool(p), yes), no)                          => if (p) yes else no
      case App(App(Identifier("*"), Integer(x)), Integer(y))   => Integer(x * y)
      case App(App(Identifier("+"), Integer(x)), Integer(y))   => Integer(x + y)
      case App(App(Identifier("+"), Floating(x)), Floating(y)) => Floating(x + y)
      case App(App(Identifier("-"), Integer(x)), Integer(y))   => Integer(x - y)
      case App(App(Identifier("%"), Integer(a)), Integer(b))   => Integer(a % b)
    }
  }

}

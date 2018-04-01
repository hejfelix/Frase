package com.lambdaminute.frase.calculus.interpreter

import com.lambdaminute.frase.calculus.ast.AST
import com.lambdaminute.frase.calculus.ast.AST.{Application => App, _}
import com.lambdaminute.frase.calculus.semantic.Keywords

object DefaultBuiltins {
  def builtIns: Keywords => BetaReduction = keyWords => {
    import AST.Syntax.AstSyntax
    import keyWords._
    {
      case (nextId, Bool(b)) =>
        val yes  = nextId
        val no   = yes.nextAvailableId
        val next = no.nextAvailableId
        (next, LambdaAbstraction(yes, LambdaAbstraction(no, if (b) yes else no)))
      case (nextId, `yCombinator`) =>
        val f             = nextId
        val x             = f.nextAvailableId
        val repeatingBody = x abs (f app (x app x))
        val ycombinator   = f abs (repeatingBody app repeatingBody)
        (x.nextAvailableId, ycombinator)
      case (nextId, App(App(Identifier("=="), a), b))                    => (nextId, Bool(a == b))
      case (nextId, App(App(Identifier("<="), Integer(a)), Integer(b)))  => (nextId, Bool(a <= b))
      case (nextId, App(App(Identifier("*"), Integer(x)), Integer(y)))   => (nextId, Integer(x * y))
      case (nextId, App(App(Identifier("+"), Integer(x)), Integer(y)))   => (nextId, Integer(x + y))
      case (nextId, App(App(Identifier("+"), Floating(x)), Floating(y))) => (nextId, Floating(x + y))
      case (nextId, App(App(Identifier("-"), Integer(x)), Integer(y)))   => (nextId, Integer(x - y))
      case (nextId, App(App(Identifier("%"), Integer(a)), Integer(b)))   => (nextId, Integer(a % b))

    }
  }

}

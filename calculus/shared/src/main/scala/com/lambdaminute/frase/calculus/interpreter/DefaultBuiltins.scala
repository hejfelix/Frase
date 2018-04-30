package com.lambdaminute.frase.calculus.interpreter

import com.lambdaminute.frase.calculus.ast.AST
import com.lambdaminute.frase.calculus.ast.AST.{Application => App, _}
import com.lambdaminute.frase.calculus.semantic.Keywords

object DefaultBuiltins {
  def builtIns: Keywords => BetaReduction = keyWords => {
    import AST.Syntax.AstSyntax
    import keyWords._
    {
      case Bool(b) =>
        val yes = Identifier("x")
        val no  = Identifier("y")
        yes abs (no abs (if (b) yes else no))
      case `yCombinator` =>
        val f             = Identifier("f")
        val x             = Identifier("x")
        val repeatingBody = x abs (f app (x app x))
        val ycombinator   = f abs (repeatingBody app repeatingBody)
        ycombinator
      case App(App(Identifier("=="), a), b)                    => Bool(a == b)
      case App(App(Identifier("<="), Integer(a)), Integer(b))  => Bool(a <= b)
      case App(App(Identifier("*"), Integer(x)), Integer(y))   => Integer(x * y)
      case App(App(Identifier("+"), Integer(x)), Integer(y))   => Integer(x + y)
      case App(App(Identifier("+"), Floating(x)), Floating(y)) => Floating(x + y)
      case App(App(Identifier("-"), Integer(x)), Integer(y))   => Integer(x - y)
      case App(App(Identifier("%"), Integer(a)), Integer(b))   => Integer(a % b)
    }
  }

}

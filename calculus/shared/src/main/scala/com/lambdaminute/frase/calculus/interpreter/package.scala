package com.lambdaminute.frase.calculus

import com.lambdaminute.frase.calculus.ast.AST.{Identifier, Term}

package object interpreter {

  type BetaReduction = PartialFunction[(Identifier, Term), (Identifier, Term)]

}

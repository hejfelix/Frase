package com.lambdaminute.frase.calculus

import com.lambdaminute.frase.calculus.ast.Ast.Term

package object interpreter {

  type BetaReduction = PartialFunction[Term, Term]

}

package com.lambdaminute.frase.calculus

import com.lambdaminute.frase.calculus.ast.AST.Term

package object interpreter {

  type BetaReduction = PartialFunction[Term, Term]

}

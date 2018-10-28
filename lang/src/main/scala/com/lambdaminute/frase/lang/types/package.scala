package com.lambdaminute.frase.lang

import com.lambdaminute.frase.calculus.ast.Ast.Term
import shapeless.tag
import shapeless.tag.@@
package object types {

  //Maker traits
  object Tags {
    trait Type
  }

  implicit class TermToType(t: Term) {
    def asType: Type = tag[Tags.Type][Term](t)
  }

  implicit class TypeToTerm(t: Type) {
    def asTerm: Term = t
  }

  type Type = Term @@ Tags.Type

}

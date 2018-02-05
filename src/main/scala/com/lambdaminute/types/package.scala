package com.lambdaminute

import com.lambdaminute.ast.AST.Term
import com.softwaremill.tagging._

package object types {

  //Maker traits
  object Tags {
    trait Type
  }

  implicit class TermToType(t: Term) {
    def asType: Type = t.taggedWith[Tags.Type]
  }

  implicit class TypeToTerm(t: Type) {
    def asTerm: Term = t
  }

  type Type = Term @@ Tags.Type

}

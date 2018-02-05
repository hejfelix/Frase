package com.lambdaminute

import com.lambdaminute.ast.AST
import com.lambdaminute.errors.FraseError

package object syntax {
  object all         extends InterpreterSyntax with UnificationSyntax with ParserSyntax {
    implicit class Sugar(s: String){

      def unify(that:String): Either[FraseError, Map[AST.Term, AST.Term]] = for {
        x <- s.parseTerm
        y <- that.parseTerm
        unifier <- x.unify(y)
      } yield unifier

      def unifyUnsafe(that:String): Map[AST.Term, AST.Term] = unify(that).right.get
    }
  }
  object interpreter extends InterpreterSyntax
  object unification extends UnificationSyntax
}

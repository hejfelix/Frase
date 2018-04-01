package com.lambdaminute.frase

import com.lambdaminute.frase.calculus.ast.AST.Term
import com.lambdaminute.frase.calculus.errors.FraseError

package object syntax {
  object all extends InterpreterSyntax with UnificationSyntax with ParserSyntax {
    implicit class Sugar(s: String) {

      def unify(that: String): Either[FraseError, List[(Term, Term)]] =
        for {
          x       <- s.parseTerm
          y       <- that.parseTerm
          unifier <- x.unify(y)
        } yield unifier

      def unifyUnsafe(that: String): List[(Term, Term)] = unify(that).right.get
    }
  }
  object interpreter extends InterpreterSyntax
  object unification extends UnificationSyntax
}

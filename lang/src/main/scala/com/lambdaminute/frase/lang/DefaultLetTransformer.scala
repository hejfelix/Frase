package com.lambdaminute.frase.lang

import com.lambdaminute.frase.calculus.ast.Ast
import com.lambdaminute.frase.calculus.errors.FraseError
import com.lambdaminute.frase.calculus.semantic.Keywords

case class DefaultLetTransformer(keywords: Keywords) extends LetTransformer {
//
//  override def transform(fragments: List[Term]): Either[FraseError, Term] = {
//
//    val namedKey   = 'NAMED
//    val termKey    = 'TERM
//    val unknownKey = 'UNKNOWN
//
//    val groups = fragments.groupBy {
//      case _: Term => termKey
//      case _       => unknownKey
//    }
//
//    val namedExpressions: Seq[Term] =
//      groups
//        .getOrElse(namedKey, Nil)
//        .collect {
//          case x @ Named(_, _) if isRecursive(x) => yCombinatorTransformation(x)
//          case x: Named                          => x
//        }
//
//    val standAloneTerms: Seq[AST.Fragment] = groups.getOrElse(termKey, Nil)
//
//    standAloneTerms match {
//      case Nil                 => Right(Empty)
//      case (term: Term) :: Nil => Right(reduce(term, namedExpressions))
//      case xs                  => Left(GenericError(s"Too many unnamed terms to evaluate: ${xs}"))
//    }
//
//  }
//
//  private def isRecursive(named: Named) =
//    named.rhs
//      .contains(named.lhs)
//
//  private def yCombinatorTransformation(named: Named): Named =
//    Named(named.lhs,
//          LambdaAbstraction(keywords.yCombinator,
//                            LambdaAbstraction(Identifier("f"), named.rhs.relabel(named.lhs, Identifier("f")))))
//
//  private def reduce(term: Term, namedExpressions: Seq[Named]): AST.Term =
//    namedExpressions.foldLeft(term) {
//      case (term, Named(id, rhs)) => Application(LambdaAbstraction(id, term), rhs)
//    }
  override def transform(fragments: List[Ast.Term]): Either[FraseError, Ast.Term] = ???
}

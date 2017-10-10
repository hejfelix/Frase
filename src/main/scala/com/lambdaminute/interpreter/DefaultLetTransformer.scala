package com.lambdaminute.interpreter

import com.lambdaminute.errors.{FraseError, GenericError}
import com.lambdaminute.semantic.Keywords
import com.lambdaminute.syntax.AST
import com.lambdaminute.syntax.AST._

case class DefaultLetTransformer(keywords: Keywords) extends LetTransformer {

  override def transform(fragments: List[Fragment]): Either[FraseError, Term] = {

    val namedKey   = 'NAMED
    val termKey    = 'TERM
    val unknownKey = 'UNKNOWN

    val groups = fragments.groupBy {
      case _: Named => namedKey
      case _: Term  => termKey
      case _        => unknownKey
    }

    val namedExpressions: Seq[Named] =
      groups
        .getOrElse(namedKey, Nil)
        .collect {
          case x @ Named(_, _) if isRecursive(x) => yCombinatorTransformation(x)
          case x: Named                          => x
        }

    val standAloneTerms: Seq[AST.Fragment] = groups.getOrElse(termKey, Nil)

    standAloneTerms match {
      case Nil                 => Right(Empty)
      case (term: Term) :: Nil => Right(reduce(term, namedExpressions))
      case xs                  => Left(GenericError(s"Too many unnamed terms to evaluate: ${xs}"))
    }

  }

  private def isRecursive(named: Named) =
    named.rhs
      .contains(named.lhs)

  private def yCombinatorTransformation(named: Named): Named =
    Named(named.lhs,
          LambdaAbstraction(keywords.yCombinator,
                            LambdaAbstraction(Identifier("f"), named.rhs.relabel(named.lhs, Identifier("f")))))

  private def reduce(term: Term, namedExpressions: Seq[Named]): AST.Term =
    namedExpressions.foldLeft(term) {
      case (term, Named(id, rhs)) => Application(LambdaAbstraction(id, term), rhs)
    }

}

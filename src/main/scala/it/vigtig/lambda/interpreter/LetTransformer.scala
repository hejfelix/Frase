package it.vigtig.lambda.interpreter

import it.vigtig.lambda.errors.{FraseError, GenericError}
import it.vigtig.lambda.syntax.AST
import it.vigtig.lambda.syntax.AST._

trait LetTransformer {

  def transform(fragments: List[Fragment]): Either[FraseError, Term]

}

case class DefaultLetTransformer() extends LetTransformer {

  override def transform(fragments: List[Fragment]): Either[FraseError, Term] = {

    val namedKey   = 'NAMED
    val termKey    = 'TERM
    val unknownKey = 'UNKNOWN

    val groups = fragments.groupBy {
      case n: Named => namedKey
      case _: Term  => termKey
      case _        => unknownKey
    }

    val namedExpressions: Seq[Fragment]    = groups.getOrElse(namedKey, Nil)
    val standAloneTerms: Seq[AST.Fragment] = groups.getOrElse(termKey, Nil)

    standAloneTerms match {
      case Nil                 => Right(Empty)
      case (term: Term) :: Nil => Right(reduce(term, namedExpressions))
      case xs                  => Left(GenericError(s"Too many unnamed terms to evaluate: ${xs}"))
    }

  }

  private def reduce(term: Term, namedExpressions: Seq[Fragment]): AST.Term =
    namedExpressions.foldLeft(term) {
      case (term, Named(id, rhs)) => Application(LambdaAbstraction(id, term), rhs)
    }

}

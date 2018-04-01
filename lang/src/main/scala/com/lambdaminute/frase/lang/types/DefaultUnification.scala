package com.lambdaminute.frase.lang.types

import com.lambdaminute.frase.calculus.interpreter.FixPoint
import com.lambdaminute.frase.calculus.ast.AST._
import com.lambdaminute.frase.calculus.errors.{FraseError, UnificationError}

class DefaultUnification(logging: Boolean = false) extends Unification with FixPoint {

  trait Step
  case object Ignore                               extends Step
  case object Fail                                 extends Step
  case class Delete(eq: (Term, Term))              extends Step
  case class Swap(eq: (Term, Term))                extends Step
  case class Eliminate(x: Term, newLabel: Term)    extends Step
  case class Decompose(newEqs: List[(Term, Term)]) extends Step

  def prettyEq(eq: (Term, Term)) =
    s"${eq._1.pretty} = ${eq._2.pretty}"

  def log(): Unit          = if (logging) println()
  def log(s: String): Unit = if (logging) { println(s) } else {}

  def prettyStep(step: Step): String = step match {
    case Delete(eq)             => s"Delete ${prettyEq(eq)}"
    case Swap(eq)               => s"Swap ${prettyEq(eq)}"
    case Eliminate(x, newLabel) => s"Eliminate `${x.pretty}` by replacement: `${newLabel.pretty}`"
    case Decompose(xs) =>
      val prettyEqs = xs.map(prettyEq).mkString(", "); s"Decomposing into { ${prettyEqs} }"
    case _ => ""
  }

  def unify(eqs: List[(Term, Term)]): Either[FraseError, List[(Term, Term)]] =
    eqs.foldLeft(Right(eqs): Either[FraseError, List[(Term, Term)]])((acc, nextEquation) => {
      val nextStep        = unify(nextEquation)
      val prettyEquations = acc.map(_.map(prettyEq)).toString
      if (nextStep != Ignore) {
        log(s"Applying step to:   ${prettyEquations}")
        log(s"  ${prettyStep(nextStep)}")
        log()
      }

      if (nextStep == Fail) {
        Left(UnificationError(s"Unable to unify ${nextEquation}"))
      } else {
        acc.map(a =>
          if (isUnification(a)) {
            a
          } else {
            applyStep(a)(nextStep).distinct
        })
      }
    })

  def isUnification(eqs: List[(Term, Term)]) =
    eqs.forall {
      case (Identifier(x), _) if isPoly(x) => true
      case _                               => false
    }

  override def unifyFix(eqs: List[(Term, Term)]): Either[FraseError, List[(Term, Term)]] =
    fixPoint(Right(eqs): Either[FraseError, List[(Term, Term)]])(_.flatMap(unify))

  def applyStep(eqs: List[(Term, Term)])(step: Step): List[(Term, Term)] = step match {
    case Decompose(newEqs) => eqs ::: newEqs
    case Swap(toBeSwapped) => eqs.map(x => if (x == toBeSwapped) toBeSwapped.swap else x)
    case Eliminate(x, newLabel) =>
      eqs.map {
        case (a, b) if a -> b != (x -> newLabel) => a.relabel(x, newLabel) -> b.relabel(x, newLabel)
        case x => x
      }
    case Delete(eq) =>
      val prettyEqs = eqs.map(prettyEq).mkString(", ")
      log(s"${prettyEq(eq)} deleting in $prettyEqs"); eqs.filter(_ != eq)
    case Ignore => eqs
    case _      => sys.error(s"Unable to perform $step")
  }

  def isMono(s: String) = !s.isEmpty && s.head.isUpper
  def isPoly(s: String) = !isMono(s)

  private def nonRedundant(equation: (Term, Term)): Boolean = equation match {
    case (x, y) => x != y
  }

  def unify(eq: (Term, Term)): Step = eq match {
    case (a, b) if a == b                                         => Delete(eq)
    case (Identifier(x), Identifier(y)) if isPoly(x) && isPoly(y) => Ignore
    case (_, Identifier(b)) if isPoly(b)                          => Swap(eq)
    case ((Application(l1, r1), Application(l2, r2))) =>
      Decompose(List(l1 -> l2, r1 -> r2).filter(nonRedundant))
    case (LambdaAbstraction(argl, bodyl), LambdaAbstraction(argr, bodyr)) =>
      Decompose(List(argl -> argr, bodyl -> bodyr).filter(nonRedundant))
    case (a @ Identifier(aa), b) if !b.vars.contains(a) && isPoly(aa) => Eliminate(a, b)
    case (Identifier(x), _) if isPoly(x)                              => Ignore
    case _                                                            => Fail
  }

}

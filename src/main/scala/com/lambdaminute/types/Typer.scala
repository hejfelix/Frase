package com.lambdaminute.types

import com.lambdaminute.syntax.AST._

case class Typer(unification: Unification, logging: Boolean = false) {

  private def log(): Unit = log("")
  private def log(s: String): Unit =
    if (logging) {
      println(s)
    }

  case class Var(id: String) {
    def increment: Var = Var((id.charAt(0) + 1).toChar.toString)
    def asTypeId: Type = Identifier(id).asType
  }

  private def builtInTypes: PartialFunction[Term, Type] = {
    case Bool(_)     => Identifier("Bool").asType
    case Floating(_) => Identifier("Float").asType
    case Integer(_)  => Identifier("Int").asType
  }

  def relabelWith(t: Term, relabels: List[(Term, Term)]) =
    relabels.foldLeft(t)((acc, relabeling: (Term, Term)) => (acc.relabel _).tupled(relabeling))

  val tab = "  "
  def infer(universe: Map[Term, Type] = Map.empty, freshVar: Var = Var("a"))(term: Term): (Map[Term, Type], Var) =
    if (builtInTypes.isDefinedAt(term)) {
      (universe + (term -> builtInTypes(term)), freshVar)
    } else {
      term match {
        case Identifier(_) =>
          (universe + (term -> freshVar.asTypeId), freshVar.increment)
        case Application(left, right) =>
          val (contextWithRightType, nextVariable) = infer(universe, freshVar)(right)
          val rightType: Type                      = contextWithRightType(right)

          val (contextWithLeftType, _) = infer(contextWithRightType, nextVariable)(left)
          val leftType1: Type          = contextWithLeftType(left)

          val argumentResultType: Type = nextVariable.asTypeId
          val leftType2: Type          = LambdaAbstraction(rightType, argumentResultType).asType

          val unifier: Either[String, List[(Term, Term)]] = unification.unifyFix(List(leftType1 -> leftType2))
          val united: Term                                = relabelWith(leftType1.asTerm, unifier.right.get)
          val LambdaAbstraction(_, expectedTermType)      = united

          log(s"[App] Found judgements for term: ${term.pretty}")
          log(s"$tab right type: ${right.pretty}: ${rightType.prettyType}")
          log(s"$tab left type1: ${left.pretty}: ${leftType1.prettyType}")
          log(s"$tab left type2: ${left.pretty}: ${leftType2.prettyType}")
          log(s"$tab Unification: ${united.prettyType}")
          log()

          (contextWithLeftType + (left -> united.asType) + (term -> expectedTermType.asType), nextVariable.increment)

        case LambdaAbstraction(arg, body) =>
          val (bodyContext, nextVariable) = infer(universe, freshVar)(body)
          val (argContext, nextVariable2) = infer(bodyContext, nextVariable)(arg)
          val inferredType: Type          = LambdaAbstraction(argContext(arg), argContext(body)).asType

          val argType  = argContext(arg)
          val bodyType = argContext(body)
          log(s"[Abs] Found judgements for term: ${term.pretty}")
          log(s"$tab arg type: ${arg.pretty}: ${argType.prettyType}")
          log(s"$tab body type: ${body.pretty}: ${bodyType.prettyType}")
          log(s"$tab term type: ${term.pretty}: ${inferredType.prettyType}")
          log()

          (argContext + (term -> inferredType), nextVariable2)
        case x => log(s"Unexpected ${x}"); ???
      }
    }

}

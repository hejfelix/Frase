package com.lambdaminute.types

import com.lambdaminute.syntax.AST._

case class Typer(logging: Boolean = false) {

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

  def variables(term: Term, acc: Map[Term, Type] = Map.empty, freshVar: Var = Var("a")): (Map[Term, Type], Var) =
    term match {
      case _ if builtInTypes.isDefinedAt(term) => (acc + (term -> builtInTypes(term)), freshVar)
      case _ if acc.contains(term)             => (acc, freshVar)
      case Identifier(_)                       => (acc + (term -> freshVar.asTypeId), freshVar.increment)
      case LambdaAbstraction(arg, body) =>
        val (r1, nextVar1) = variables(arg, acc, freshVar)
        val (r2, nextVar2) = variables(body, r1, nextVar1)
        (r2 + (term -> nextVar2.asTypeId), nextVar2.increment)

      case Application(left, right) =>
        val (r1, nextVar1) = variables(left, acc, freshVar)
        val (r2, nextVar2) = variables(right, r1, nextVar1)
        (r2 + (term -> nextVar2.asTypeId), nextVar2.increment)

      case _ => sys.error("EXPLOSION")
    }

  def expandMap(universe: Map[Term, Type], freshVar: Var = Var("a")): (Map[Term, Type], Var) =
    universe.toList.foldLeft((universe, freshVar))((acc, judgement) => expand(acc._1, judgement, acc._2))

  def relabelWith(t: Term, relabels: Map[Term, Term]) =
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

          val unifiedLeftType: Map[Term, Term] =
            leftType1.unify(leftType2).fold(sys.error, identity)

          val relabeledLeftType: Type                = relabelWith(leftType1, unifiedLeftType).asType
          val LambdaAbstraction(_, expectedTermType) = relabeledLeftType.asTerm

          log(s"[App] Found judgements for term: ${term.pretty}")
          log(s"$tab right type: ${right.pretty}: ${rightType.prettyType}")
          log(s"$tab left type1: ${left.pretty}: ${leftType1.prettyType}")
          log(s"$tab left type2: ${left.pretty}: ${leftType2.prettyType}")
          log(s"$tab Result from unification: ${relabeledLeftType.prettyType}")
          log(s"$tab unification map: ${unifiedLeftType}")
          log()

          (contextWithLeftType + (left -> relabeledLeftType) + (term -> expectedTermType.asType),
           nextVariable.increment)

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

  /**
    * Precondition: All subterms in the keys of universe have matching keys in the universe
    * @param universe
    * @param judgement
    * @param freshVar
    * @return
    */
  private def expand(universe: Map[Term, Type], judgement: (Term, Type), freshVar: Var): (Map[Term, Type], Var) =
    judgement match {
      case (Identifier(_), _) =>
        (universe, freshVar)
      case (app @ Application(left, right), _) =>
        val appResultType = freshVar.asTypeId
        val newLeftType   = LambdaAbstraction(universe(right), appResultType).asType
        (universe.updated(left, newLeftType).updated(app, appResultType), freshVar.increment)
      case (abs @ LambdaAbstraction(arg @ Identifier(_), body), _) if body.freeVars.contains(arg) =>
        val argType: Type  = universe(arg)
        val bodyType: Type = universe(body)
        val absType: Type  = LambdaAbstraction(argType.asTerm, bodyType.asTerm).asType
        (universe.updated(abs, absType), freshVar)
      case (abs @ LambdaAbstraction(Identifier(_), body), _) =>
        val argType: Type  = freshVar.asTypeId
        val bodyType: Type = universe(body)
        val absType: Type  = LambdaAbstraction(argType.asTerm, bodyType.asTerm).asType
        (universe.updated(abs, absType), freshVar.increment)
      case _ => (universe, freshVar)
    }

}

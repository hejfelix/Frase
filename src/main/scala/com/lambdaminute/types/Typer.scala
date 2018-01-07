package com.lambdaminute.types

import com.lambdaminute.syntax.AST._

case class Typer() {

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
      case (abs @ LambdaAbstraction(arg, body), _) =>
        val argType: Type  = universe(arg)
        val bodyType: Type = universe(body)
        val absType: Type  = LambdaAbstraction(argType.asTerm, bodyType.asTerm).asType
        (universe.updated(abs, absType), freshVar)
      case _ => (universe, freshVar)
    }

}

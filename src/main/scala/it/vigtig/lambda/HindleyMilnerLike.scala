package it.vigtig.lambda

import com.typesafe.scalalogging.StrictLogging

trait HindleyMilnerLike extends ASTLike with StrictLogging {

  val FUNC: String = "Func"

  trait Type
  trait TMono extends Type
  trait TPoly extends Type

  case object TNothing           extends Type
  case object TUndefined         extends Type
  case class TFail(str: String)  extends Type
  case class TInst(name: String) extends TMono
  case class TVar(name: String)  extends TMono
  object TPolyInst {
    def apply(name: String): TPolyInst           = TPolyInst(name, TNothing, TNothing)
    def apply(name: String, in: Type): TPolyInst = TPolyInst(name, in, TNothing)
  }
  case class TPolyInst(name: String, in: Type, out: Type) extends TPoly
  //scalastyle:off
  def TFunc(in: Type, out: Type): TPolyInst = TPolyInst(FUNC, in, out)
  //scalastyle:on

  type Context = Map[Term, Type]

  def prettyType(t: Type): String = t match {
    case TPolyInst(FUNC, in, TNothing) => s"${prettyType(in)}"
    case TPolyInst(name, in, TNothing) => s"$name ${prettyType(in)}"
    case TPolyInst(FUNC, in, out)      => s"${prettyType(in)} -> ${prettyType(out)}"
    case TPolyInst(name, in, out)      => s"""$name ${prettyType(in)} -> ${prettyType(out)}"""
    case TVar(x)                       => x
    case TInst(x)                      => x
    case _                             => t.toString
  }

  private def log(s: String): Unit =
    logger.info(s)

  /*
  https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system#Free_type_variables
   */
  def freeVars(t: Type): Set[Type] = t match {
    case TVar(_)               => Set(t)
    case TInst(_)              => Set()
    case TPolyInst(_, in, out) => freeVars(in) ++ freeVars(out)
//      args map freeVars reduce (_ ++ _)
  }

  /*
  https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system#Context_and_typing
   */
  def freeVars(ctx: Context): Set[Type] =
    ctx map {
      case (variable, tpe) => freeVars(tpe)
    } reduce (_ ++ _)

//  /*
//  https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system#Context_and_typing
//  */
//  def freeInContext(alpha: Type, ctx: Context) =
//    ctx exists {
//      case (variable, tpe) => alpha == tpe
//    }

  /*
  https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system#Polymorphic_type_order
   */
  def moreGeneralOrEqual(a: Type, b: Type, ctx: Context): Boolean = (a, b) match {
    case (TNothing, TNothing) => true
    case (TVar(a), _)         => true
    case (TInst(x), TInst(y)) => x == y
    case (a: TPolyInst, b: TPolyInst) =>
      a.name == b.name &&
        moreGeneralOrEqual(a.in, b.in, ctx) && moreGeneralOrEqual(a.out, b.out, ctx)
  }

  def unifyInContext(ctx: Context, map: Map[Type, Type]): Context =
    ctx.mapValues(tpe => map.getOrElse(tpe, tpe))

  def expandType(t: Type, assignments: Map[Type, Type]): Type =
    t match {
      case TVar(_)                  => assignments.getOrElse(t, t)
      case TPolyInst(name, in, out) => TPolyInst(name, expandType(in, assignments), expandType(out, assignments))
      case _                        => t
    }

  //Cyclomatic complexity ignored here
  //scalastyle:off
  /*
  https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system#Degrees_of_freedom_instantiating_the_rules
   */
  def unify(a: Type,
            b: Type,
            ctx: Context,
            assignments: Map[Type, Type] = Map.empty): (Type, Context, Map[Type, Type]) =
    (a, b) match {
      //Both variables

      case (TNothing, TNothing) => (TNothing, ctx, assignments)
      case (TVar(_), TVar(_)) =>
        val subst = assignments.getOrElse(a, a)
        (subst, unifyInContext(ctx, Map(b -> subst)), Map(b -> subst))

      //Assign inst to var
      case (TVar(_), _) => (b, unifyInContext(ctx, Map(a -> b)), Map(a -> b))
      case (_, TVar(_)) => (a, unifyInContext(ctx, Map(b -> a)), Map(b -> a))

      //Instances unify syntactically
      case (TInst(x), TInst(y)) =>
        if (x != y) {
          (TFail(a + " != " + b), ctx, assignments)
        } else {
          (a, ctx, assignments)
        }

      //Polytypes unify if type arguments have same length and unify pairwise
      case (a: TPolyInst, b: TPolyInst) if a.name != b.name =>
        (TFail(s"name mismatch: ${a.name} != ${b.name}"), ctx, assignments)
      case (a: TPolyInst, b: TPolyInst) =>

        println(s"poly unify with poly ${prettyType(a)}     ${prettyType(b)}")

        val (newInType, newCtx, newAss)      = unify(a.in, b.in, ctx, assignments)
        val (newOutType, finalCtx, finalAss) = unify(a.out, b.out, newCtx, newAss)

        println(s"newass:  $newAss    finalAss $finalAss")

        println(s" new intype: ${prettyType(newInType)}    new outtype:  ${newOutType}")
        val expandedType                     = expandType(TPolyInst(a.name, newInType, newOutType), newAss ++ finalAss)

        println(s" expanded type ${prettyType(expandedType)}")
        (expandedType, finalCtx, finalAss)

//      val substitutions: Map[Type, Type] = (a.args, b.args).zipped.map(unifySub).toMap
//
//      //We don't use mapValues because it's not strict
//      val sub2 = substitutions.map { case (k, v) => k -> subInType(v, substitutions) }
//
//      val ctx2 = ctx.mapValues(x => subInType(x, substitutions))
//
//      val pairs = (a.args, b.args).zipped.map((x, y) => unify(x, y, ctx)._1)
//
//      val newArgs: Seq[Type] = pairs.map(x => subInType(x, substitutions))
//      log(s"arg lists: ${a.args}      ${b.args}")
//      log(s"substitutions: ${substitutions}")
//      log(s"sub2: ${sub2}")
//      log(s"unified types: ${pairs.mkString}")
//      log(s"newArgs: ${newArgs}")
//
//      val unifiedContext: Context = unifyInContext(ctx2, sub2)
//      log(s"unifiedcontext: $unifiedContext")
//      log("")
//
//      (TPolyInst(a.name, newArgs: _*), unifiedContext)
      case _ => (TFail(s"$a could not unify with $b"), ctx, assignments)
    }
  //scalastyle:on

//  def subInType(t: Type, subs: Map[Type, Type]): Type = t match {
//    case tp: TPolyInst               => TPolyInst(tp.name, tp.args.map(x => subInType(x, subs)): _*)
//    case x: TVar if subs.contains(x) => subs(x)
//    case x                           => x
//  }

  def unifySub(a: Type, b: Type): (Type, Type) = (a, b) match {
    case (TVar(_), TVar(_)) if a == b => (TFail(""), TFail(""))
    case (TVar(_), TVar(_))           => b -> a
    case (TInst(_), TVar(_))          => b -> a
    case (TVar(_), TInst(_))          => a -> b
    case _                            => (TFail(""), TFail(""))
  }

  def nextId(s: String, inc: Int = 1): String = "" + (s.charAt(0) + inc).toChar

  def boolType(nextVar: String): (TPolyInst, String) = {
    val argt = TVar(nextVar)
    (TPolyInst(FUNC, argt, TPolyInst(FUNC, argt, argt)), nextId(nextVar))
  }

  val A = Applic

  val knownTypes: PartialFunction[(Term, String, Context), (Type, String, Context)] = {
    case (Integer(_), next, ctx) => (TInst("Int"), next, ctx)
    case (Bit(_), next, ctx) =>
      val (bool, next2) = boolType(next)
      (bool, next2, ctx)
    case (Floating(_), next, ctx) => (TInst("Float"), next, ctx)
    case (Id("*"), next, ctx)     =>
      //a -> a -> a
      val argt = TVar(next)
      (TPolyInst(FUNC, argt, TPolyInst(FUNC, argt, argt)), nextId(next), ctx)

    case (Id("+"), next, ctx) =>
      val argt = TVar(next)
      (TPolyInst(FUNC, argt, TPolyInst(FUNC, argt, argt)), nextId(next), ctx)

    case (Id("-"), next, ctx) =>
      val argt = TVar(next)
      (TPolyInst(FUNC, argt, TPolyInst(FUNC, argt, argt)), nextId(next), ctx)

    //if/else branching
    case (A(A(Bit(_), y), z), next, ctx) =>
      unifyWithContext(y, z, next, ctx)

    case (Id("<="), next, ctx) =>
      val tvar          = TVar(next)
      val (bool, next2) = boolType(nextId(next))
      (TPolyInst(FUNC, tvar, TPolyInst(FUNC, tvar, bool)), next2, ctx)
  }

  def unifyWithContext(y: Term, z: Term, next: String, ctx: Context): (Type, String, Context) = {
    val (yt, idPlus, ctx2)          = w2(y, ctx, next)
    val (zt, idPlusPlus, ctx3)      = w2(z, ctx2, idPlus)
    val (commonBranchType, ctx4, _) = unify(yt, zt, ctx3)
    (commonBranchType, idPlusPlus, ctx4)
  }

  //cyclomatic complexity ignored here
  //scalastyle:off
  /*
    https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system#Algorithm_W
   */
  def w2(e: Term, ctx: Context, nextVar: String): (Type, String, Context) = e match {
    case _ if knownTypes.isDefinedAt((e, nextVar, ctx)) => knownTypes((e, nextVar, ctx))
    case Named(name, body) =>
      log(s"[Named] $name = $body")
      val (bodyType, next2, newContext) = w2(body, ctx, nextVar)
      (newContext.getOrElse(name, bodyType), next2, newContext)
    case Id(_) if (ctx.contains(e)) => (ctx(e), nextVar, ctx)

    case SetId(id) if (ctx.contains(e)) => (ctx(e), nextVar, ctx)

    case Id(_) =>
      log(s"[Var] ${prettyStr(e)} : ${prettyType(TVar(nextVar))}")
      (TVar(nextVar), nextId(nextVar), ctx + (e -> TVar(nextVar)))

    case Applic(e0, e1) =>
      val tauPrime            = TVar(s"_$nextVar")
      val (tau0, next, ctx2)  = w2(e0, ctx, nextId(nextVar))
      val (tau1, next2, ctx3) = w2(e1, ctx2, next)
      //Does tau0 unify with tau1 -> tauPrime?
      unify(tau0, TPolyInst(FUNC, tau1, tauPrime), ctx3) match {
        case tt @ (TPolyInst(_, _, out), newCtx, _) =>
          val logText = "[App] %s : %s with e0 %s : %s and e1 %s  : %s      ----  verdict: %s".format(
            prettyStr(e),
            prettyType(out),
            prettyStr(e0),
            prettyType(tau0),
            prettyStr(e1),
            prettyType(tau1),
            (prettyStr(e) ->
              prettyType(out)))

          log(logText)
          log(s"[APP] ctx: ${newCtx}")

          (out, next2, newCtx + (e -> out))
        case (TFail(x), _, _) => (TFail(x), next, ctx)
      }

    case Abstr(id, e) =>
      val (tau, next, ctx2)       = w2(id, ctx, nextVar)
      val (tauPrime, next2, ctx3) = w2(e, ctx2, next)
      log(s"[ABSTR] ctx2:$ctx2   ctx3:$ctx3")
      if (isFailed(tau) || isFailed(tauPrime)) {
        (TFail("Type check failed"), next, ctx)
      } else {
        log(s"tau: $tau   tauPrime:$tauPrime")
        val fromType = ctx3.getOrElse(id, tau)
        val toType   = ctx3.getOrElse(e, tauPrime)
        val res      = TPolyInst(FUNC, fromType, toType)
        log(s"[Abstr] ${prettyStr(e)} : ${prettyType(res)}")
        (res, next2, ctx3 + (e -> res))
      }
    case _ => (TFail(""), nextVar, ctx)
  }
  //scalastyle:on

  def isFailed(t: Type): Boolean = t match {
    case TFail(_) => true
    case _        => false
  }

  def inst(sigma: Type, next: String): (Type, String) =
    sigma match {
      case TInst(_) => (sigma, next)
      case TVar(_)  => (TVar(next), nextId(next))
      case TPolyInst(name, in, out) =>
        val (newInTypeInst, nextVar)   = inst(in, next)
        val (newOutTypeInst, finalVar) = inst(out, nextVar)
        (TPolyInst(name, newInTypeInst, newOutTypeInst), finalVar)
//      case TPolyInst(name, args @ _ *) =>
//        val varargs = (args collect { case TVar(x) => TVar(x) }).distinct
//        val map: Map[Type, TVar] = varargs.zipWithIndex.map {
//          case (variable, index) => variable -> TVar(nextId(next, index + 1))
//        }.toMap
//
//        log(varargs.zipWithIndex.mkString(","))
//        log(s"MAP $map from $sigma")
//        (TPolyInst(name, args.map(x => map.getOrElse(x, x)): _*), nextId(next, args.length))
    }

  def newTyper(next: String = "a"): (Term) => (Type, String, Context) = { (e: Term) =>
    {
      w2(e, Map(), next)
    }
  }

}

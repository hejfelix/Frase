package it.vigtig.lambda

/**
  * @author Hargreaves
  */
trait HindleyMilnerLike extends
  ASTLike {

  val FUNC:String = "Func"

  abstract trait Type

  abstract trait TMono extends
    Type

  abstract trait TPoly extends
    Type

  case class TFail(str:String) extends Type

  case class TInst(name:String) extends TMono

  case class TVar(name:String) extends TMono

  case class TPolyInst(name:String, args:Type*) extends TPoly

  case class TFunc(in:Type, out:Type) extends TPoly

  type Context = Map[Term, Type]

  def prettyType(t:Type):String = t match {
    case TPolyInst(FUNC, args@_*) => s"""${args.map(prettyType).mkString(" -> ") }"""
    case TPolyInst(name, args@_*) => s"""$name ${args.map(prettyType).mkString(" ") }"""
    case TFunc(in, out)           => s"${prettyType(in) } -> ${prettyType(out) }"
    case TVar(x)                  => x
    case TInst(x)                 => x
    case _                        => t.toString
  }

  val VERBOSE = true

  def log(s:String) = if (VERBOSE) {
    println(s)
  }

  /*
  https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system#Free_type_variables
  */
  def freeVars(t:Type):Set[Type] = t match {
    case TVar(_)                => Set(t)
    case TInst(_)               => Set()
    case TPolyInst(_, args@_ *) => args map freeVars reduce (_ ++ _)
  }

  /*
  https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system#Context_and_typing
  */
  def freeVars(ctx:Context):Set[Type] =
    ctx map {
      case (variable, tpe) => freeVars(tpe)
    } reduce (_ ++ _)

  /*
  https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system#Context_and_typing
  */
  def freeInContext(alpha:Type, ctx:Context) =
    ctx exists {
      case (variable, tpe) => alpha == tpe
    }

  /*
  https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system#Polymorphic_type_order
  */
  def moreGeneralOrEqual(a:Type, b:Type, ctx:Context):Boolean = (a, b) match {
    case (TVar(a), _)               => true
    case (TInst(x), TInst(y))       => x == y
    case (a:TPolyInst, b:TPolyInst) =>

      a.name == b.name &&
      (a.args, b.args)
        .zipped
        .map(moreGeneralOrEqual(_, _, ctx))
        .foldLeft(true)(_ && _)

  }


  def unifyInContext(ctx:Context, map:Map[Type, Type]) =
    ctx.mapValues(tpe => map.getOrElse(tpe, tpe))

  /*
  https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system#Degrees_of_freedom_instantiating_the_rules
   */
  def unify(a:Type, b:Type, ctx:Context):(Type, Context) = (a, b) match {
    //Both variables
    case (TVar(_), TVar(_)) => (a, unifyInContext(ctx, Map(b -> a)))

    //Assign inst to var
    case (TVar(_), _) => (b, unifyInContext(ctx, Map(a -> b)))
    case (_, TVar(_)) => (a, unifyInContext(ctx, Map(b -> a)))

    //Instances unify syntactically
    case (TInst(x), TInst(y)) => if (x != y) {
      (TFail(a + " != " + b), ctx)
    } else {
      (a, ctx)
    }

    //Polytypes unify if type arguments have same length and unify pairwise
    case (a:TPolyInst, b:TPolyInst) if a.name != b.name               =>
      (TFail(s"name mismatch: ${a.name } != ${b.name }"), ctx)
    case (a:TPolyInst, b:TPolyInst) if a.args.length != b.args.length =>
      (TFail(s"arg length mismatch: ${a.args.length } != ${b.args.length }"), ctx)
    case (a:TPolyInst, b:TPolyInst)                                   =>
      val substitutions:Map[Type, Type] = (a.args, b.args)
        .zipped
        .map(unifySub)
        .toMap



      //log(s"a.args: ${a.args.mkString}  b.args: ${b.args.mkString}")

      val pairs = (a.args, b.args)
        .zipped
        .map((x, y) => unify(x, y, ctx)._1)

      val newArgs = pairs
        .map(x => substitutions.getOrElse(x, x))

      //      log(s"pairs:  ${pairs.mkString}")
      //      log(s"old args:   ${a.args}    new args:  ${newArgs}")
      log(
           s"""Unified variables:   ${
             substitutions.map(x => prettyType(x._1) + "/" + prettyType(x._2))
               .mkString("  ")
           }"""
         )
      (TPolyInst(a.name, newArgs:_*), unifyInContext(ctx, substitutions))
  }

  def unifySub(a:Type, b:Type):(Type, Type) = (a, b) match {
    case (TVar(_), TVar(_)) if a == b => (TFail(""), TFail(""))
    case (TInst(_), TVar(_))          => b -> a
    case (TVar(_), TInst(_))          => a -> b
    case (TVar(_), _)                 => b -> a
    case (_, TVar(_))                 => a -> b
    case _                            => (TFail(""), TFail(""))
  }


  def nextId(s:String, inc:Int = 1):String = "" + (s.charAt(0) + inc).toChar

  def boolType(nextVar:String) = {

    val argt = TVar(nextVar)
    (TPolyInst(FUNC, argt, TPolyInst(FUNC, argt, argt)), nextId(nextVar))
  }

  val A = Applic

  val knownTypes:PartialFunction[(Term, String, Context), (Type, String, Context)] = {
    case (Integer(_), next, ctx)                        => (TInst("Int"), next, ctx)
    case (Bit(_), next, ctx)                            => (TInst("Bool"), next, ctx)
    case (Floating(_), next, ctx)                       => (TInst("Float"), next, ctx)
    case (Id("*"), next, ctx)                           =>
      //a -> a -> a
      val argt = TVar(next)
      (TPolyInst(FUNC, argt, TPolyInst(FUNC, argt, argt)), nextId(next), ctx)
    case (Id("-"), next, ctx)                           =>
      val argt = TVar(next)
      (TPolyInst(FUNC, argt, TPolyInst(FUNC, argt, argt)), nextId(next), ctx)

      //if/else branching
    case (A(A(Bit(_), y), z), next, ctx)                =>
      unifyWithContext(y, z, next, ctx)

    case (A(A(A(Id("<="), x), y), z), next, ctx)        =>
      unifyWithContext(y, z, next, ctx)

    case (Id("<="), next, ctx)                          =>
      val tvar = TVar(next)
      val (bool, next2) = boolType(nextId(next))
      (TPolyInst(FUNC, tvar, TPolyInst(FUNC, tvar, bool)), nextId(next2), ctx)
  }

  def unifyWithContext(y:Term, z:Term, next:String, ctx:Context):(Type, String, Context) = {
    val (yt, idPlus, ctx2) = w2(y, ctx, next)
    val (zt, idPlusPlus, ctx3) = w2(z, ctx2, idPlus)
    val (commonBranchType, ctx4) = unify(yt, zt, ctx3)
    (commonBranchType, idPlusPlus, ctx4)
  }

  /*
    https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system#Algorithm_W
     */
  def w2(e:Term, ctx:Context, nextVar:String):(Type, String, Context) = e match {
    case _ if knownTypes.isDefinedAt((e, nextVar, ctx)) => knownTypes((e, nextVar, ctx))
    case Named(_, body)                                 => w2(body, ctx, nextVar)
    case Id(_) if (ctx.contains(e))                     => (ctx(e), nextVar, ctx)

    case Id(_)          =>
      log(s"[Var] ${prettyStr(e) } : ${prettyType(TVar(nextVar)) }")
      (TVar(nextVar), nextId(nextVar), ctx + (e -> TVar(nextVar)))
    case Applic(e0, e1) =>
      val tauPrime = TVar(nextVar)
      val (tau0, next, ctx2) = w2(e0, ctx, nextId(nextVar))
      val (tau1, next2, ctx3) = w2(e1, ctx2, next)
      //Does tau0 unify with tau1 -> tauPrime?
      val (TPolyInst(_, _, out), newCtx) = unify(tau0, TPolyInst(FUNC, tau1, tauPrime), ctx3)
      log(s"""[App] ${prettyStr(e) } : ${prettyType(out) }    with e0 ${prettyStr(e0) } : ${prettyType(tau0) } and e1
            ${prettyStr(e1) }  : ${prettyType(tau1) }    verdict: ${
        prettyStr(e) ->
        prettyType(out)
      }"""
         )
      (out, next2, newCtx + (e -> out))
    case Abstr(id, e)   =>
      val (tau, next, ctx2) = w2(id, ctx, nextVar)
      val (tauPrime, next2, ctx3) = w2(e, ctx2, next)
      val res = TPolyInst(FUNC, tau, tauPrime)
      log(s"[Abstr] ${prettyStr(e) } : ${prettyType(res) }")
      (res, next2, ctx3 + (e -> res))
    case _              => (TFail(""), nextVar, ctx)
  }

  def inst(sigma:Type, next:String):(Type, String) =
    sigma match {
      case TInst(_)                 => (sigma, next)
      case TVar(_)                  => (TVar(next), nextId(next))
      case TPolyInst(name, args@_*) =>
        val varargs = (args collect { case TVar(x) => TVar(x) }).distinct
        val map:Map[Type, TVar] =
          varargs
            .zipWithIndex
            .map { case (variable, index) => variable -> TVar(nextId(variable.name, index + 1)) }
            .toMap

        log(s"MAP $map from $sigma")
        (TPolyInst(name, args.map(x => map.getOrElse(x, x)):_*), nextId(next, args.length))
    }

  def newTyper(next:String = "a") = {
    (e:Term) => {
      w2(e, Map(), next)
    }
  }


}





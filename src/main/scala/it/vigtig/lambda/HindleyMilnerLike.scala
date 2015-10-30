package it.vigtig.lambda

/**
 * @author Hargreaves
 */
trait HindleyMilnerLike extends ASTLike {

  abstract trait Type

  abstract trait TMono extends Type

  abstract trait TPoly extends Type

  case class TFail(str:String) extends Type

  case class TInst(name: String) extends TMono

  case class TVar(name: String) extends TMono

  case class TPolyInst(name: String, args: Type*) extends TPoly

  case class TFunc(in: Type, out: Type) extends TPoly

  type Context = List[(Term,Type)]

  def prettyType(t: Type): String = t match {
    case TFunc(in, out) => s"${prettyType(in)} -> ${prettyType(out)}"
    case TVar(x)        => x
    case TInst(x)       => x
    case _              => t.toString
  }

  /*
  https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system#Free_type_variables
  */
  def freeVars(t:Type):Set[Type] = t match {
    case TVar(_) => Set(t)
    case TInst(_) => Set()
    case TPolyInst(_,args @ _ *) => args map freeVars reduce (_++_)
  }

  /*
  https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system#Context_and_typing
  */
  def freeVars(ctx:Context):Set[Type] =
    ctx map {
      case (variable,tpe) => freeVars(tpe)
    } reduce (_++_)

  /*
  https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system#Context_and_typing
  */
  def freeInContext(alpha:Type,ctx:Context) =
    ctx exists {
      case (variable,tpe) => alpha==tpe
    }

  /*
  https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system#Polymorphic_type_order
  */
    def moreGeneralOrEqual(a:Type,b:Type,ctx:Context):Boolean = (a,b) match
    {
      case (TVar(a),_) => true
      case (TInst(x),TInst(y)) => x == y
      case (a:TPolyInst,b:TPolyInst) =>

        a.name==b.name &&
          (a.args,b.args)
            .zipped
            .map(moreGeneralOrEqual(_,_,ctx))
            .foldLeft(true)(_&&_)

    }

  /*
  https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system#Degrees_of_freedom_instantiating_the_rules
   */
    def unify(a:Type,b:Type):Type = (a,b) match
    {
      //Both variables
      case (TVar(_),TVar(_)) => a

      //Assign inst to var
      case (TVar(_),TInst(_)) => b
      case (TInst(_),TVar(_)) => a

      //Instances unify syntactically
      case (TInst(x),TInst(y)) => if(x!=y) TFail(a+" != "+b) else a

      //Polytypes unify if type arguments have same length and unify pairwise
      case (a:TPolyInst,b:TPolyInst) =>
        if(a.name!=b.name)
          TFail("name mismatch: "+a.name+" != "+b.name)
        else if (a.args.length != b.args.length)
          TFail("arg length mismatch: "+a.args.length +"!="+ b.args.length)
        else {
          val substitutions = (a.args,b.args).zipped.map( unifySub).toMap
          val newArgs = (a.args, b.args).zipped
            .map(unify)
            .map(x => substitutions.getOrElse(x,x))
          TPolyInst(a.name, newArgs: _*)
        }
  }

  def unifySub(a: Type, b: Type): (Type, Type) = (a, b) match {
    case (TVar(_), TInst(_)) => a -> b
    case (TInst(_), TVar(_)) => b -> a
    case _                   => (TFail(""), TFail(""))
  }


    /*
    https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system#Algorithm_W
     */
    def w2(e:Term,ctx:Context,newVar: () => String):Type = e match {

      case Integer(_)        => TInst("Int")
      case Bit(_)            => TInst("Bool")
      case Floating(_)       => TInst("Float")

      case Id(_) if(ctx.exists(_._1 == e)) => ctx.find(_._1 == e).get._2

      case Id(_) => TVar(newVar())

      case Applic(e0,e1) =>
        val tauPrime = TVar(newVar())
        val tau0 = w2(e0,ctx,newVar)
        val tau1 = w2(e1,ctx,newVar)
        //Does tau0 unify with tau1 -> tauPrime?
        val TPolyInst(name,in,out) = unify(tau0,TPolyInst("Func",tau1,tauPrime))
        out

      case Abstr(id,e) =>
        val tau = TVar(newVar())
        val tauPrime = w2(e,(id -> tau) :: ctx,newVar)
        TPolyInst("Func",tau,tauPrime)


      case _ => TFail("")
    }

    def inst(sigma:Type,newVar: () => String):Type =
      sigma match {
        case TInst(_) => sigma
        case TVar(_) => TVar(newVar())
        case TPolyInst(name,args @ _* ) =>
          TPolyInst(name,args map (t => inst(t,newVar)) :_*)
      }










  /*
  Old code
   */
  def newTyper = {
    var nextVar = 'a'
    W(Map[Term, Type]().withDefault { t =>
      val v = nextVar
      nextVar = (nextVar + 1).toChar
      TVar("" + v)
    })
  }

  def min(a: Type, b: Type) = a match {
    case _: TInst => a
    case _        => b
  }

  def substitute(t: Type, sub: (Type, Type)): Type = (t, sub) match {
    case (x, (y, z)) if x == y => z
    case (TFunc(in, out), _)   => TFunc(substitute(in, sub), substitute(out, sub))
    case fallback              => t
  }

  private def knownTypes(context: Map[Term, Type]): PartialFunction[Term, Type] = {
    case Integer(_)        => TInst("Int")
    case Bit(_)            => TInst("Bool")
    case Floating(_)       => TInst("Float")
    case SetId(sid)        => TInst(sid)
    case Id("+")           => val t = context(Empty); TFunc(t, TFunc(t, t))
    case Id("-")           => val t = context(Empty); TFunc(t, TFunc(t, t))
    case Id("/")           => val t = context(Empty); TFunc(t, TFunc(t, t))
    case Id("*")           => val t = context(Empty); TFunc(t, TFunc(t, t))
    case Id("<=")          => val t = context(Empty); TFunc(TInst("Bool"), TFunc(t, TFunc(t, t)))
    case Applic(Bit(p), x) => val t = W(context)(x); TFunc(t, t)
  }

  private def W(context: Map[Term, Type]): PartialFunction[Term, Type] =
    knownTypes(context) orElse {

      case Id(x) =>
        context(Id(x))

      case Named(id, body) =>
        val idTyp = W(context)(id)
        W(context + (id -> idTyp))(body)

      case a@Applic(Abstr(i, e), t) =>
        val iType = W(context)(i)
        val tType = W(context)(t)
        val eType = W(context + (i -> min(tType, iType)))(e)
        println("Applic abstr: " + context.mkString(","))
        val result = eType match {
          case TFunc(in, out) => out
          case _              => eType
        }
        result

      case term@Applic(a, t) =>
        val tType = W(context)(t)
        val aType = W(context + (t -> tType))(a)
        println("Applic: " + context.mkString(",") + "  " + a + "  " + t)
        println("Applic: " + prettyType(aType) + "  " + prettyType(tType))
        val result = aType match {
          case TFunc(in, out) =>
            if (in != tType && !in.isInstanceOf[TVar] && !tType.isInstanceOf[TVar]){
              val message = prettyType(in) + " != " + prettyType(tType) + " in " + prettyStr(term)
              System.err.println(message)
             TFail(message)
            } else {
              val sub = if(in.isInstanceOf[TVar]) in -> tType else if(tType.isInstanceOf[TVar]) tType -> in else TInst("a") -> TInst("a")
              substitute(out,sub)
            }
          case _              => aType
        }
        if (aType == TInst("Bool"))
          tType
        else
          result

      case a@Abstr(i, e) =>
        val iType = W(context)(i)
        val eType = W(context + (i -> iType))(e)
        eType match {
          case TFunc(_, res) => TFunc(iType, res)
          case _             => TFunc(iType, eType)
        }

      case term =>
        context(term) //Fallback
    }

}





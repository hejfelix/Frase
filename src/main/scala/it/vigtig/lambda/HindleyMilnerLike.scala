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

  def prettyType(t: Type): String = t match {
    case TFunc(in, out) => s"${prettyType(in)} -> ${prettyType(out)}"
    case TVar(x)        => x
    case TInst(x)       => x
    case _              => t.toString
  }

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





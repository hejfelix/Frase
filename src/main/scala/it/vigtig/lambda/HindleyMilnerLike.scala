package it.vigtig.lambda

/**
 * @author Hargreaves
 */
trait HindleyMilnerLike {
  import AST._
  abstract trait Type
  abstract trait TMono extends Type
  abstract trait TPoly extends Type

  case class TInst(name: String) extends TMono
  case class TVar(name: String) extends TMono
  case class TPolyInst(name: String, args: Type*) extends TPoly

  case class TFunc(in: Type, out: Type) extends TPoly
  case class TFunc2(a: Type, b: Type, out: Type) extends TPoly

  def prettyType(t:Type):String = t match {
    case TFunc(in,out) => s"${prettyType(in)} -> ${prettyType(out)}"
    case TVar(x) => x
    case TInst(x) =>  x
    case _ => t.toString
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

  private def knownTypes(context: Map[Term, Type]): PartialFunction[Term, Type] = {
    case Integer(_)          => TInst("Int")
    case Bit(_)              => TInst("Bool")
    case Floating(_)         => TInst("Float")
    case Applic(Id("+"), x)  => val t=W(context)(x);TFunc(t,t)
    case Applic(Id("-"), x)  => val t=W(context)(x);TFunc(t,t)
    case Applic(Id("<="), x) => val t=W(context)(x);val y=W(context)(Empty);TFunc(t, TInst("Bool"))
    case Applic(Bit(p),x) => val t=W(context)(x);TFunc(t, t)
  }

  private def W(context: Map[Term, Type]): PartialFunction[Term, Type] =
    knownTypes(context) orElse {

      case Id(x) =>
        context(Id(x))

      case Named(id, body) =>
        val idTyp = W(context)(id)
        W(context + (id -> idTyp))(body)

      case a@Applic(Abstr(i,e),t) => 
        val iType = W(context)(i)
        val tType = W(context)(t)
        val eType = W(context+(i -> min(tType, iType)))(e)
        val result = eType match{
          case TFunc(in,out) => out
          case _ => eType
        }
        result
        
      case term@Applic(a, t) =>
        val tType = W(context)(t)
        val aType = W(context+(t -> tType))(a)
        val result =aType match {
          case TFunc(in,out) => 
            if(in!=tType && !(in.isInstanceOf[TVar])) 
              System.err.println(in+" != "+tType+" in "+prettyStr(term))
            out
          case _ => aType
        }
        if(aType==TInst("Bool"))
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





package it.vigtig.lambda

/**
 * @author Hargreaves
 */
trait Typer {
  import it.vigtig.lambda.LambdaAST._

  abstract trait Type
  abstract trait TMono extends Type
  abstract trait TPoly extends Type

  case class TInst(name: String) extends TMono
  case class TVar(name: String) extends TMono
  case class TPolyInst(name: String, args: Type*) extends TPoly

  case class TFunc(in: Type, out: Type) extends TPoly
  case class TFunc2(a: Type, b: Type, out: Type) extends TPoly

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
    case Applic(Id("<="), x) => val t=W(context)(x);TFunc(t, TFunc(t, TInst("Bool")))
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
        }
        result
        
      case term@Applic(a, t) =>
        val tType = W(context)(t)
        val aType = W(context+(t -> tType))(a)
        println("applic2> "+prettyStr(a)+": "+aType)
        aType

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

object TyperTest extends Typer
    with Parser
    with InterpreterLike
    with App {
  import it.vigtig.lambda.LambdaAST._

  parseAll(LINE, "fib = n . (<= n 1) (1) ((+ (fib (- n 2)) (fib (- n 1))))") match {
    case Success(term, _) =>
      println(prettyStr(term))
      println()
      println("AST:" + term)
      println("Type:" + newTyper(term))
    case _ =>
  }

}




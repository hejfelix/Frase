package it.vigtig.lambda

/**
 * @author Hargreaves
 */
trait Typer {
  import it.vigtig.lambda.LambdaAST._

  abstract trait Type
  case class TMono(name: String) extends Type
  case class TPoly(name: String, args: Type*) extends Type
  case class TFunc(in: Type, out: Type) extends Type
  case class TFunc2(a: Type, b: Type, out: Type) extends Type

  def newTyper = {
    var nextVar = 'a'
    hindleyMilner(Map[Term, Type]().withDefault { t =>
      val v = nextVar
      nextVar = (nextVar + 1).toChar
      TMono("" + v)
    })
  }

  private def knownTypes(context: Map[Term, Type]): PartialFunction[Term, Type] = {
    case Integer(_)  => TMono("Int")
    case Bit(_)      => TMono("Bool")
    case Floating(_) => TMono("Float")
    case Applic(Id("+"), x) =>
      val typX = context(x)
      TFunc(typX, TFunc(typX,typX))
  }

  private def hindleyMilner(context: Map[Term, Type]): PartialFunction[Term, Type] =
    knownTypes(context) orElse {
    
      case Id(x) =>
        
        println("Id:"+context+"   "+x)
        context(Id(x))
        
      case Applic(a, t) =>
        val tType =hindleyMilner(context)(t)
        val aType =hindleyMilner(context+(t ->tType ))(a)
        println("applic:"+a+"   "+aType+"    "+tType)
        aType match {
          case TFunc(_, res)     => println(res); res
        }
        
      case Abstr(i, e) =>
        
        val iType = hindleyMilner(context)(i)
        val eType = hindleyMilner(context + (i -> iType))(e)
        println("Abstr: "+Abstr(i,e))
        println("Abstr:"+context + (i -> iType)+" i:"+i+"  eType:"+eType)
        eType match{
          case TFunc(_,res) => TFunc(iType,res)
          case _ => TFunc(iType,eType)
        }

        

      case term =>
        println("Fallback: "+term)
        context(term) //Fallback
    }

}

object TyperTest extends Typer
    with Parser
    with InterpreterLike
    with App {
  import it.vigtig.lambda.LambdaAST._

  parseAll(LINE, "x . y") match {
    case Success(term, _) =>
      println("AST:" + term)
      println("Type:" + newTyper(term))
    case _ =>
  }

}




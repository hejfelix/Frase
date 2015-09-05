package it.vigtig.lambda
/**
 * @author Hargreaves
 */
object REPL
    extends ParserLike
    with InterpreterLike
    with HindleyMilnerLike 
    with ASTLike {

  def main(args: Array[String]) = loop()

  def loop(context: Map[Term, List[Term]] = Map().withDefaultValue(Nil)): Unit = {

    def time(b: => Unit): Long = {
      val t = System.currentTimeMillis()
      b
      System.currentTimeMillis() - t
    }

    val exprSrc = io.StdIn.readLine("Frase>")
    parseAll(LINE, exprSrc) match {
      case Success(expr, _) =>
        
        
        val definition:List[(Term,Term)] = expr match {
          case Named(id, body) =>
            println(s"""added "${id.id}" to context""")
            List(id -> body)
          case SetType(Id(setId),vars,cons) =>
            println("added constructor(s) to context:") 
            cons map {
              case ConstructorDef(Id(id),args) => 
                val consBody = 
                  parseAll(LINE,s"""${args.map(t => t._1).mkString(".")} ${if(args != Nil)"." else ""} $id ${args.map(t=>t._1).mkString(" ")}""")
                  match {
                  case Success(term,_) => term
                  case x => System.err.println("Couldn't parse "+x); sys.error("hej")
                  }
                Id(id) -> consBody
            }
          case _ => Nil
        }


        println()
        println(s"Parsed:       ${prettyStr(expr)} : ${prettyType(newTyper(expr))}")
        println(s"AST:          $expr")
        println(s"context:      $context")
        val evalTime = time {
          println("Evaluated:    " + prettyStr(interpret(expr)(context)))
        }
        println(s"time:         $evalTime ms")
        println()

        loop(combine(context,listToMap(definition)))

      case err: NoSuccess => println(err)
    }

  }
  
  def combine(a:Map[Term,List[Term]],b:Map[Term,List[Term]]) = 
    (a.keys ++ b.keys).map(i => i -> (a.getOrElse(i, Nil) ++ b.getOrElse(i, Nil))).toMap
}
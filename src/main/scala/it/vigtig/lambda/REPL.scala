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

  def loop(context: Map[Id, Term] = Map()): Unit = {

    def time(b: => Unit): Long = {
      val t = System.currentTimeMillis()
      b
      System.currentTimeMillis() - t
    }

    val exprSrc = io.StdIn.readLine("Frase>")
    parseAll(LINE, exprSrc) match {
      case Success(expr, _) =>
        
        
        val definition:List[(Id,Term)] = expr match {
          case Named(id, body) =>
            println(s"""added "${id.id}" to context""")
            List((id -> body))
          case SetType(Id(setId),vars,cons) => 
            cons map {
              case Constructor(Id(id),args) => 
                println(s"""$id = ${args.map(t => t._1).mkString(".")} ${if(args != Nil)"." else ""} $id ${args.map(t=>t._1).mkString(" ")}""")
                val consBody = 
                  parseAll(LINE,s"""${args.map(t => t._1).mkString(".")} ${if(args != Nil)"." else ""} $id ${args.map(t=>t._1).mkString(" ")}""")
                  match {
                  case Success(expr,_) => expr
                  case x => System.err.println("Couldn't parse "+x); error("hej")
                  }
                println(Named(Id(id),consBody))   
                (Id(id) -> consBody)
            }
          case _ => Nil
        }


        println()
        println(s"Parsed:       ${prettyStr(expr)} : ${prettyType(newTyper(expr))}")
        println(s"AST:          $expr")
        val evalTime = time {
          println("Evaluated:    " + prettyStr(interpret(expr)(context)))
        }
        println(s"time:         $evalTime ms")
        println()

        loop(context ++ definition)

      case err: NoSuccess => println(err)
    }

  }
}
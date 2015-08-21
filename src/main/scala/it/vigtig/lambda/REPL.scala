package it.vigtig.lambda
/**
 * @author Hargreaves
 */
object REPL 
extends ParserLike
with InterpreterLike 
with ASTLike {
  def main(args: Array[String]) = loop()
 
  def loop(context:Map[Id,Term] = Map()):Unit = {
      val exprSrc = io.StdIn.readLine("Frase>")
      parseAll(LINE,exprSrc) match {
        case Success(expr, _) => 
          
          val definition = expr match {
            case Named(id,body) => 
              println(s"""added "${id.id}" to context""")
              Some(id -> body)
            case _ => None
          }
          println()
          println(s"Parsed:  ${prettyStr(expr)}")
          println(s"AST: $expr")
          println("Evaluated: "+prettyStr(interpret(expr)(context)))
          println()
          
          loop(context ++ definition)
          
        case err: NoSuccess   => println(err)
      }
      
  }
}
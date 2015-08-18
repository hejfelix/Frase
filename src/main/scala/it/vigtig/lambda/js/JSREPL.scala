package it.vigtig.lambda.js
import scala.scalajs.js.JSApp
import it.vigtig.lambda._

/**
 * @author Felix
 */
object JSREPL extends JSApp
with Parser
with InterpreterLike{
  def main(): Unit = {
    loop()
  }
  
   
  def loop():Unit = {
      val exprSrc = "(a . true a) b"
      parseAll(TERM,exprSrc) match {
        case Success(expr, _) => 
          println(expr)
          println("Parsed: " + prettyStr(expr))
//          println("Evaluated: "+prettyStr(interpret(expr)))
////          println()
        case err: NoSuccess   => println(err)
      }
  }
}
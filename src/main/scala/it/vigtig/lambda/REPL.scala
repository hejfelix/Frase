//package it.vigtig.lambda
///**
// * @author Hargreaves
// */
//object REPL 
//extends Parser
//with InterpreterLike {
//  def main(args: Array[String]) = loop()
// 
//  def loop():Unit = {
//      val exprSrc = io.StdIn.readLine("Frase>")
//      parseAll(LINE,exprSrc) match {
//        case Success(expr, _) => 
//          println("Parsed: " + prettyStr(expr))
//          println("Evaluated: "+prettyStr(interpret(expr)))
//          println()
//        case err: NoSuccess   => println(err)
//      }
//      loop()
//  }
//}
package it.vigtig.lambda
/**
 * @author Hargreaves
 */
object REPL
    extends ParserLike
    with InterpreterLike
    with HindleyMilnerLike {
    import AST._
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

        val definition = expr match {
          case Named(id, body) =>
            println(s"""added "${id.id}" to context""")
            Some(id -> body)
          case _ => None
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
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

  def first[A, B](t: (A, B)) = t match {
    case (x, _) => x
  }

  def second[A, B](t: (A, B)) = t match {
    case (_, x) => x
  }

  def time(b: => Unit): Long = {
    val t = System.currentTimeMillis()
    b
    System.currentTimeMillis() - t
  }

  def loop(context: Map[Term, List[Term]] = Map().withDefaultValue(Nil),nextVar:String = "a"): Unit = {

    val exprSrc = io.StdIn.readLine("Frase>")
    parseAll(LINE, exprSrc) match {
      case Success(expr, _) =>

        val definition: List[(Term, Term)] = expr match {
          case Named(id, body)                =>
            println( s"""added "${id.id}" to context""")
            List(id -> body)
          case SetType(Id(setId), vars, cons) =>
            println("added constructor(s) to context:")
            cons map {
              case ConstructorDef(Id(id), args) =>
                val cons = s"""${args.map(first).mkString(".")} ${if (args != Nil) "." else ""}"""
                val consTail = s"""$id ${args.map(first).mkString(" ")}"""
                val consBody = parseAll(LINE, cons + consTail)
                match {
                  case Success(term, _) => term
                  case x                => System.err.println("Couldn't parse " + x); sys.error("hej")
                }
                Id(id) -> consBody
            }
          case _                              => Nil
        }

        val (typeOfExpression,nextVariable) = w2(expr,Map(),nextVar)
        println()
        println(s"Parsed:       ${prettyStr(expr)} : ${prettyType(typeOfExpression)}")
        val evalTime = time {
          println("Evaluated:    " + prettyStr(interpret(expr)(context)))
        }
        println(s"time:         $evalTime ms")
        println()
        loop(combine(context, listToMap(definition)),nextVariable)

      case err => println(err)
    }

  }

  def combine(a: Map[Term, List[Term]], b: Map[Term, List[Term]]) =
    (a.keys ++ b.keys).map(i => i -> (a.getOrElse(i, Nil) ++ b.getOrElse(i, Nil))).toMap
}
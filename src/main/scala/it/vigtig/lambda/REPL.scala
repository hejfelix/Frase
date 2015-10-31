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

  def loop(context: Map[Term, List[Term]] = Map().withDefaultValue(Nil),
           typeContext: Map[Term,Type] = Map(),
           nextVar:String = "a"): Unit = {

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

        val (typeOfExpression,nextVariable,newTypeCtx) = w2(expr,typeContext,nextVar)

        val namedType:Map[Term,Type] = expr match {
          case Named(id,body) =>
            if(typeContext.contains(id))
              Map(id -> unify(typeOfExpression,typeContext(id),newTypeCtx)._1)
            else
              Map(id -> typeOfExpression)
          case _ => Map()
        }

        println()
        val typeOfExpr = if(namedType.isEmpty) typeOfExpression else namedType.values.head
        println(s"Parsed:       ${prettyStr(expr)} : ${prettyType(typeOfExpr)}")
        val evalTime = time {
          println("Evaluated:    " + prettyStr(interpret(expr)(context)))
        }
        println(s"time:         $evalTime ms")
        println()


        println("NEW TYPE CONTEXT: "+newTypeCtx.map(x => prettyStr(x._1)+" : "+prettyType(x._2)).mkString("\n"))
        loop(combine(context, listToMap(definition)),newTypeCtx ++ namedType,nextVariable)

      case err => println(err)
    }

  }

  def combine(a: Map[Term, List[Term]], b: Map[Term, List[Term]]) =
    (a.keys ++ b.keys)
      .map(i => i -> (a.getOrElse(i, Nil) ++ b.getOrElse(i, Nil)))
      .toMap
}
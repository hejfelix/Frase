package it.vigtig.lambda

import com.typesafe.scalalogging.StrictLogging

import scala.annotation.tailrec

object REPL extends ParserLike with InterpreterLike with HindleyMilnerLike with ASTLike with StrictLogging {

  def main(args: Array[String]): Unit = {

    if (args.length == 1) {
      val file = io.Source.fromFile(args(0)).getLines()
      val std  = io.StdIn
      val ctx  = loop(input = { file.next() })
      loop()
    } else {
      loop()
    }
  }

  def first[A, B](t: (A, B)): A = t match {
    case (x, _) => x
  }

  def second[A, B](t: (A, B)): B = t match {
    case (_, x) => x
  }

  def time[T](b: => T): (Long, T) = {

    val t      = System.currentTimeMillis()
    val result = b
    (System.currentTimeMillis() - t, result)
  }

  case class InvalidProgramException(message: String) extends Exception

  def parseType(str: String): Type = parseAll(LINE, str) match {
    case Success(expr, _) =>
      expr match {
        case Id(x)                         => TVar(x)
        case Applic(SetId(set), Id(varId)) => TPolyInst(set, TVar(varId))
      }
    case _ => TFail(s"Could not parse type in: $str")
  }

  //scalastyle:off
  //for cyclomatic complexity

  //Not that we're going to, but let's not run out of stack
  @tailrec
  def loop(context: Map[Term, List[Term]] = Map().withDefaultValue(Nil),
           typeContext: Map[Term, Type] = Map(),
           nextVar: String = "a",
           input: => String = io.StdIn.readLine("Frase>")): Map[Term, List[Term]] = {

    logger.info("CURRENT TYPE CONTEXT: " + typeContext.map(kvp => kvp._1 -> prettyType(kvp._2)).mkString(",  "))

    logger.info("CURRENT TYPE CONTEXT: " + typeContext.mkString("\n"))

    val exprSrc = input
    if (exprSrc == ":exit") {
      context
    } else {
      parseAll(LINE, exprSrc) match {
        case Success(expr, _) =>
          val definition: List[(Term, Term)] = expr match {
            case Named(id, body) =>
              logger.info(s"""added "${id.id}" to context""")
              List(id -> body)
            case SetType(Id(setId), vars, cons) =>
              logger.info("added constructor(s) to context:")
              cons map {
                case ConstructorDef(Id(id), args) =>
                  logger.info(s"NEW TYPE:  $id:$setId")
                  val cons = s"""${args.map(first).mkString(".")} ${if (args != Nil) {
                    "."
                  } else {
                    ""
                  }}"""
                  val consTail = s"""$id ${args.map(first).mkString(" ")}"""
                  logger.info(s"all args: ${args.mkString}")
                  logger.info(s"parsing: ${cons + consTail}")
                  val consBody = parseAll(LINE, cons + consTail) match {
                    case Success(term, _) => term
                    case _                => Empty
                  }
                  logger.info(("constructor: " + Id(id) -> consBody).toString)

                  Id(id) -> consBody
              }
            case term => Nil
          }

          val setTypeDefs = expr match {
            case SetType(Id(setId), vars, cons) =>
              cons map {
                case ConstructorDef(Id(id), args) =>
                  val varTypes  = vars.map(x => TVar(x.id): Type)
                  val argTypes  = args.map(_._2).map(parseType)
                  val out: Type = TPolyInst(setId, varTypes: _*)
                  SetId(id) -> TPolyInst(FUNC, argTypes :+ out: _*)
              }
            case _ => Nil
          }

          val (typeOfExpression, nextVariable, newTypeCtx) = w2(expr, typeContext, nextVar)
          logger.info("AST: " + expr)

          val namedType: Map[Term, Type] = expr match {
            case Named(id, body) =>
              if (typeContext.contains(id)) {
                Map(id -> unify(typeOfExpression, typeContext(id), newTypeCtx)._1)
              } else {
                Map(id -> typeOfExpression)
              }
            case _ => Map()
          }

          logger.info("")
          val typeOfExpr = if (namedType.isEmpty) {
            typeOfExpression
          } else {
            namedType.values.head
          }

          val (evalTime, result) = time { interpret(expr)(context) }

          typeOfExpr match {
            case TFail(err) if !expr.isInstanceOf[SetType] =>
              logger.info(s"Parsed:       ${prettyStr(expr)}")
              logger.info("Type error: " + err)
              loop(context, typeContext, nextVariable, input)
            case tpe =>
              logger.info(s"Parsed:       ${prettyStr(expr)} : ${prettyType(typeOfExpr)}")
              logger.info("Evaluated:    " + prettyStr(result))
              logger.info(s"time:         $evalTime ms")
              logger.info("")
              //            logger.info("NEW TYPE CONTEXT: "+newTypeCtx.map(x => prettyStr(x._1)+" : "+prettyType(x._2))
              // .mkString("\n"))

              loop(combine(context, listToMap(definition)),
                   newTypeCtx ++ namedType ++ setTypeDefs,
                   nextVariable,
                   input)
          }

        case err => logger.info(err.toString); throw new InvalidProgramException(err.toString);
      }

    }
  }

  def combine(a: Map[Term, List[Term]], b: Map[Term, List[Term]]) =
    (a.keys ++ b.keys).map(i => i -> (a.getOrElse(i, Nil) ++ b.getOrElse(i, Nil))).toMap
}

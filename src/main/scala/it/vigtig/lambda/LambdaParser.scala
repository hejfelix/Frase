package it.vigtig.lambda

import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.PackratParsers
import scala.language.postfixOps

/**
 * @author Hargreaves
 */
class LambdaParser extends RegexParsers with PackratParsers {
  
    lazy val identifier:PackratParser[String]  = """[_\p{L}][_\p{L}\p{Nd}]*""".r ^^ {_.toString}
    
    lazy val TERM:PackratParser[LambdaTerm] = (LABSTR | APP | ID )
      
    lazy val ID:PackratParser[Id] = identifier ^^ { Id(_:String) }
    
    lazy val LABSTR:PackratParser[LambdaAbstraction] = ID ~ "." ~ TERM ^^
      { case id ~ _ ~ term => LambdaAbstraction(id,term) }
    
    lazy val APP:PackratParser[Application] = TERM ~ TERM ^^
      { case t1 ~ t2 => Application(t1,t2) }
    
}

abstract trait LambdaTerm
case class Id(id:String) extends LambdaTerm
case class LambdaAbstraction(id:Id,body:LambdaTerm) extends LambdaTerm
case class Application(left:LambdaTerm,right:LambdaTerm) extends LambdaTerm

object TestLambdaParser extends LambdaParser with App {
  parseAll(TERM, "x . y z") match {
    case Success(lup,_) => println(lup)
    case x => println(x)
  }
}
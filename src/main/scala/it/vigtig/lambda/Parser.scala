package it.vigtig.lambda

import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.PackratParsers
import scala.language.postfixOps

/**
 * @author Hargreaves
 */
trait Parser extends RegexParsers with PackratParsers {
  import LambdaAST._
  
    lazy val identifier:PackratParser[String]  = 
      """[a-z]""".r ^^ {_.toString}
    
    lazy val TERM:PackratParser[Term] = EXPR | PEXPR
    
    lazy val PEXPR:PackratParser[Term] = "("~EXPR~")" ^^ 
      { case _ ~ term ~ _ => term }
  
    lazy val EXPR:PackratParser[Term] = (APP | LABSTR  | ID) 
      
    lazy val ID:PackratParser[Id] = 
      identifier ^^ { Id(_:String) }
    
    lazy val LABSTR:PackratParser[Abstraction] = 
      ID ~ "." ~ TERM ^^
        { case id ~ _ ~ term => Abstraction(id,term) }
    
    lazy val APP:PackratParser[Application] = 
      TERM ~ TERM ^^
        { case t1 ~ t2 => Application(t1,t2) }
    
}


object LambdaAST {
  
  abstract trait Term 
  case class Id(id:String) extends Term
  case class Abstraction(id:Id,body:Term) extends Term
  case class Application(left:Term,right:Term) extends Term
  
}

object TestLambdaParser extends Parser with App {
  parseAll(TERM, "( x . y z) a") match {
    case Success(lup,_) => println(lup)
    case x => println(x)
  }
}
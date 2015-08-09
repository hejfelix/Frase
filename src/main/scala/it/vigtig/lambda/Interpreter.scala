package it.vigtig.lambda

/**
 * @author Hargreaves
 */
object Interpreter extends Parser with App {
  import LambdaAST._
  
  val TEST1 = "(y . x . y z x) a b"
  
  parseAll(TERM, TEST1) match {
    case Success(lup,_) => println(lup); println(interpret(lup))
    case x => println(x)
  }
  
  def interpret(term:Term) = reduce(term)
  
  def reduce(t:Term):Term = t match {
    case Id(_) => t
    case Application(Abstraction(id,body),rhs) => 
      reduce(betaReduce(rhs,id,body))
    case _ => t
  }
  
  def betaReduce(newX:Term, oldX:Id, in:Term): Term = in match {
    case `oldX` => newX
    case Id(_) => in
    case Abstraction(id,body) => Abstraction(id, betaReduce(newX, oldX, body))
    case Application(l,r) => Application(betaReduce(newX, oldX, l), betaReduce(newX, oldX, r))
  }
 
}
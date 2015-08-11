package it.vigtig.lambda

/**
 * @author Hargreaves
 */
object Interpreter extends Parser with App {
  import LambdaAST._
  
  val TEST1 = "( y . x . y z x ) a b"
  val TEST2 = "a b c d"
  
  def prettyPrintln(t:Term):Unit = {prettyPrint(t);println()}
  def prettyPrint(t:Term):Unit = t match {
    case Application(a,b) => 
      print("(")
      prettyPrint(a)
      print(" ")
      prettyPrint(b) 
      print(")")
    case Abstraction(x,b) => 
      prettyPrint(x)
      print(".")
      prettyPrint(b)
    case Id(x) => print(x)
    case _ => print(t)
  }
  
  parseAll(TERM, TEST1) match {
    case Success(lup,_) => 
      prettyPrintln(lup)
      println("  beta-reduction ->")
      prettyPrintln(interpret(lup))
    case x => println(x)
  }
  
  def interpret(term:Term) = betaReduction(term)
  
  def reduce(t:Term):Term = t match {
    case Id(_) => t
    case Application(Abstraction(id,body),rhs) => 
      reduce(betaReduce(rhs,id,body))
    case Application(t,y) => Application(reduce(t),reduce(y))
    case Abstraction(a,b) => Abstraction(a,reduce(b))
    case _ => t
  }
  
  def fixPoint[T](t:T)(p:T => T):T =
    if(p(t) != t)
      fixPoint(p(t))(p)
    else
      t
      
  def betaReduction(t:Term):Term = fixPoint(t)(reduce)
  
  def betaReduce(newX:Term, oldX:Id, in:Term): Term = in match {
    case `oldX` => newX
    case Id(_) => in
    case Abstraction(id,body) => 
      Abstraction(id, betaReduce(newX, oldX, body))
    case Application(l,r) => 
      Application(betaReduce(newX, oldX, l), betaReduce(newX, oldX, r))
  }
 
}
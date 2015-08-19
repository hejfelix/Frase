package it.vigtig.lambda

/**
 * @author Hargreaves
 */

trait InterpreterLike {
  import LambdaAST._
  def interpret(t: Term)(context:Map[Id,Term] = Map()): Term 
    = fixPoint(t)(evalStep(context))

   def show[T](t:T):T = {
    println(t)
    t
  }
  
  def evalStep(context:Map[Id,Term])(t:Term) = 
     fixPoint(resolve(t)(context))(reducer)
  

  def resolve(t:Term)(context:Map[Id,Term]):Term = context.foldLeft(t)((a,b) => substitute(a)(b))
  
  def size(t: Term): Int = t match {
    case Id(_)                        => 1
    case Named(id, body)              => 1 + size(id) + size(body)
    case Applic(t, y)                 => 1 + size(t) + size(y)
    case Abstr(a, b)                  => 1 + size(a) + size(b)
    case _                            => 1
  }

  def builtIns:PartialFunction[Term,Term] = {
    case Applic(Applic(Id("=="),a),b) => Bit(a==b)
    case Applic(Applic(Id("*"),Integer(x)),Integer(y)) => Integer(x*y)
    case Applic(Applic(Id("+"),Integer(x)),Integer(y)) => Integer(x+y)
    case Applic(Applic(Id("-"),Integer(x)),Integer(y)) => Integer(x-y)
    case Applic(Applic(Bit(p),yes),no) => if(p) yes else no
    case Applic(Applic(Id("%"),Integer(a)),Integer(b)) => Integer(a%b)
    case Applic(Applic(Id("<="),Integer(a)),Integer(b)) => Bit(a<=b)
  }

  def reducer = {
      def betaReduce:PartialFunction[Term,Term] = builtIns orElse {
      case Named(id, body)              => Named(id, betaReduce(body))
      case Applic(Abstr(id, body), rhs) => betaReduce(substitute(body)(id -> rhs))
      case Applic(t, y)                 => Applic(betaReduce(t), betaReduce(y))
      case Abstr(a, b)                  => Abstr(a, betaReduce(b))
      case i@Id(_)                      => i
      case t                            => t
    }
      betaReduce
  }
 

  def prettyStr(t: Term): String = t match {
    case Applic(a @ Id(_), b) => s"${prettyStr(a)} ${prettyStr(b)}"
    case Applic(a, b)         => s"(${prettyStr(a)}) (${prettyStr(b)})"
    case Abstr(Id(x), b)      => s"$x . ${prettyStr(b)}"
    case Id(x)                => x
    case Named(Id(x), term)   => s"$x = ${prettyStr(term)}"
    case Empty                => "< >"
    case Integer(i)           => i.toString
    case Floating(f)          => f.toString
    case Bit(b)               => b.toString
  }

  def fixPoint[T](t: T)(p: T => T): T ={
    if (p(t) != t)
      fixPoint(p(t))(p)
    else
      t
  }

  def freeVars(t: Term): Set[Id] = t match {
    case a @ Id(_)       => Set(a)
    case Abstr(id, body) => freeVars(body) - id
    case Applic(a, b)    => freeVars(a) ++ freeVars(b)
    case Empty           => Set()
    case Named(id, term) => freeVars(term)
    case _         => Set()
  }

  //Capture-avoiding substitution
  def substitute(t: Term)(label: (Id, Term)): Term = (t, label) match {
    case (Empty,_) => Empty
    case (i: Id, (j, k)) if i == j => k
    case (i: Id, _)                => i
    case (a:Atom, _)               => a
    case (Applic(a, b), _)         => Applic(substitute(a)(label), substitute(b)(label))
    case (Abstr(id, body), (x, y)) if id != x && !(freeVars(y)(id)) =>
      Abstr(id, substitute(body)(label))
    case (a @ Abstr(_, _), _) => a
    case _ => t
  }

}

object Interpreter extends Parser with InterpreterLike with App {
  import LambdaAST._

  val TEST1 = "( y . (x . y z x ) a) b"
  val TEST2 = "a b c d"
  val TEST3 = "a = + 1 2 true - .32f"
  val TEST_AMB = "(x . x . x) (y 42)"

  val TESTNAMED =
    """
      double = x . (+ x x)
      double 4
      """

  val TESTFIB = """
    fib = n . (<= n 1) (1) ((+ (fib (- n 2)) (fib (- n 1))))

    fib 0
    fib 1
    fib 2
    fib 3
    fib 4
    fib 5
    fib 6
    fib 7
    """
  
  val TESTSUB = """
    x = a b c
    y = d e f
    z = g h i
    
    x y z
    """
  
  val TESTFAC = """
    fac = n . (<= n 1) (1) (* (n) (fac (- n 1)))
    
    fac 1 
    fac 2
    fac 3
    fac 4
    fac 5
    fac 6
    """
  
  val label = Id("y") -> Id("y")

  parseAll(PRGM, TESTFIB) match {
    case Success(lup, _) =>
      val (nameds,unnameds) = lup filter (_!=Empty) partition {
        case n: Named => true
        case _        => false
      }
      

      val dict:Map[Id,Term] = nameds.map {
        case Named(a, b) => a -> b
      }.toMap

      println("program:\n"+(lup filter (_!=Empty) map prettyStr).mkString("\n"))
      println()
      
      println("Evaluating...\n"+unnameds.map(t => {
        val res = interpret(t)(dict)
        interpret(res)()
      }).map(prettyStr).mkString("\n"))
      
      
    case x => println(x)
  }

}
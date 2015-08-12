package it.vigtig.lambda

import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.PackratParsers
import scala.language.postfixOps

/**
 * @author Hargreaves
 */
trait Parser 
  extends RegexParsers 
  with PackratParsers  {
  import LambdaAST._
  
  type PParser[T] = PackratParser[T]
  
  override val whiteSpace = """[ ]+""".r

  lazy val BIT:PParser[Bit] = """(true|false)""".r ^^ {
    case "true" => Bit(true)
    case "false" => Bit(false)
  }
  
  lazy val INTEGER:PParser[Integer] = 
    """-?\d+""".r ^^ 
    {i => Integer(i.toInt)}
  
  lazy val FLOAT:PParser[FloatingPoint] =
    """-?(\d+(\.\d*)?|\d*\.\d+)([eE][+-]?\d+)?[fFdD]?""".r ^^ 
      {f => FloatingPoint(f.toFloat)}
  
  lazy val identifier: PParser[String] =
    """[a-zA-Z+\-\\/\*]+""".r ^^ { _.toString }
  
  lazy val ATOM:PParser[Atom] = BIT | INTEGER | FLOAT | ID

  lazy val PRGM:PParser[List[Term]] = (LINE | EMPTYLINE).*
  
  lazy val EMPTYLINE = EOL ^^ {_ => Empty}
  
  lazy val EOF:PParser[String] = """\z""".r ^^ identity
  lazy val EOL:PParser[String] = sys.props("line.separator").r ^^ identity
  
  lazy val LINE:PParser[Term] = (NAMED | TERM) <~ (EOL | EOF)
  
  lazy val NAMED:PParser[Named] = 
    ID ~ ("=" ~> TERM) ^^
     { case id ~ body => Named(id,body) }
  
  lazy val TERM: PParser[Term] = LABSTR | APP | ATOM | PEXPR

  lazy val PEXPR: PParser[Term] = "(" ~> TERM <~ ")"

  lazy val ID: PParser[Id] =
    identifier ^^ Id

  lazy val LABSTR: PParser[Abstraction] =
    ID ~ "." ~ TERM ^^
      { case id ~ _ ~ term => Abstraction(id, term) }

  lazy val APP: PParser[Term] =
    TERM ~ ATOM ^^
      { case t1 ~ t2 => Application(t1, t2) }
  
}

object LambdaAST {

  abstract trait Term
  
  case object Empty extends Term
  
  case class Abstraction(id: Id, body: Term) extends Term
  case class Application(left: Term, right: Term) extends Term
  case class Named(id:Id,body:Term) extends Term
  
  abstract trait Atom extends Term //"Normal Form", term cannot be reduced further
  case class Id(id: String) extends Atom
  case class Integer(i:Int) extends Atom
  case class FloatingPoint(f:Float) extends Atom
  case class Bit(b:Boolean) extends Atom
}

object TestLambdaParser extends Parser with App {
  parseAll(TERM, "( x . y z) a") match {
    case Success(lup, _) => println(lup)
    case x               => println(x)
  }
}

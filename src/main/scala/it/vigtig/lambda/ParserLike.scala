package it.vigtig.lambda

import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.PackratParsers
import scala.language.postfixOps

/**
 * @author Hargreaves
 */
trait ParserLike
    extends RegexParsers
    with PackratParsers {
  import AST._

  type PParser[T] = PackratParser[T]

  lazy val BIT: PParser[Bit] = """(true|false)""".r ^^ {
    case "true"  => Bit(true)
    case "false" => Bit(false)
  }

  lazy val INTEGER: PParser[Integer] =
    """-?\d+""".r ^^
      { i => Integer(i.toInt) }

  lazy val FLOAT: PParser[Floating] =
    """-?(\d+(\.\d*)?|\d*\.\d+)([eE][+-]?\d+)?[fFdD]?""".r ^^
      { f => Floating(f.toFloat) }

  lazy val VARIABLE: PParser[String] =
    """[a-z+\-\\/\*=%<][a-zA-Z]*""".r ^^ identity
  
  lazy val SET: PParser[String] = 
    """[A-Z][a-z]*""".r ^^ identity
    
  lazy val SET_DEF: PParser[SetType] = 
    (("""set""".r ~> SET ~ ("""[a-z]""".r.*) <~ """=""".r) ~SET_INSTANCE.+) ^^ { 
    case name ~ vars ~ is => 
      SetType(Id(name),vars.map(Id),is.map(s => Constructor(Id(s),Nil)))
    }
  
  lazy val SET_INSTANCE:PParser[String] = 
    (SET~ """[a-z]""".r.*) ^^ { _.toString}

    
    
  lazy val ATOM: PParser[Atom] = BIT | INTEGER | FLOAT | ID

  lazy val PRGM: PParser[List[Term]] = (LINE | EMPTYLINE).*

  lazy val EMPTYLINE = EOL ^^ { _ => Empty }

  lazy val EOF: PParser[String] = """\z""".r ^^ identity
  lazy val EOL: PParser[String] = sys.props("line.separator").r ^^ identity

  lazy val LINE: PParser[Term] = (SET_DEF | NAMED | TERM ) <~ (EOL | EOF)

  lazy val NAMED: PParser[Named] =
    ID ~ ("=" ~> TERM) ^^
      { case id ~ body => Named(id, body) }

  lazy val TERM: PParser[Term] =  LABSTR | APP | ATOM | PEXPR   

  lazy val PEXPR: PParser[Term] = "(" ~> TERM <~ ")"

  lazy val ID: PParser[Id] =
    VARIABLE ^^ Id

  lazy val LABSTR: PParser[Abstr] =
    ID ~ "." ~ TERM ^^
      { case id ~ _ ~ term => Abstr(id, term) }

  lazy val APP: PParser[Term] =
    TERM ~ (PEXPR | ATOM ) ^^
      { case t1 ~ t2 => Applic(t1, t2) }

}



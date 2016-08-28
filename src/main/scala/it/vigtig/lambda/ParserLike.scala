package it.vigtig.lambda

import scala.language.postfixOps
import scala.util.parsing.combinator.{PackratParsers, RegexParsers}

trait ParserLike extends RegexParsers with PackratParsers with ASTLike {

  type PParser[T] = PackratParser[T]

  override val whiteSpace = """[ ]+""".r

  lazy val BIT: PParser[Bit] = """(true|false)""".r ^^ {
    case "true"  => Bit(true)
    case "false" => Bit(false)
  }

  lazy val INTEGER: PParser[Integer] =
    """-?\d+""".r ^^ { i =>
      Integer(i.toInt)
    }

  lazy val FLOAT: PParser[Floating] =
    """-?(\d+(\.\d*)?|\d*\.\d+)([eE][+-]?\d+)?[fFdD]?""".r ^^ { f =>
      Floating(f.toFloat)
    }

  lazy val VARIABLE: PParser[String] =
    """<=|(?!or)(?!set)[a-z+\-\\/\*=%<][a-zA-Z]*""".r ^^ identity

  lazy val SET: PParser[String] =
    """[A-Z][a-zA-Z]*""".r ^^ identity

  lazy val SET_DEF: PParser[SetType] =
    ("""set""".r ~> SET ~ """[a-z]""".r.* <~ """=""".r) ~ SET_DEF_RHS ^^ {
      case name ~ vars ~ constructors =>
        SetType(Id(name), vars.map(Id), constructors)
    }

  lazy val SET_DEF_RHS: PParser[List[ConstructorDef]] =
    SET_INSTANCE ~ ("""or""".r ~> SET_INSTANCE).* ^^ {
      case i ~ is => i :: is
    }

  lazy val LIST: PParser[Term] =
    "[" ~> TERM ~ ("," ~> TERM).* <~ "]" ^^ {
      case head ~ tail => listToCons(head :: tail)
    }

  def listToCons(xs: List[Term]): Term = xs match {
    case Nil     => SetId("Nil")
    case x :: xs => Applic(Applic(SetId("Cons"), x), listToCons(xs))
  }

  lazy val SET_INSTANCE: PParser[ConstructorDef] =
    SET ~ SET_ARGS.? ^^ {
      case name ~ Some(variables) => ConstructorDef(Id(name), variables)
      case name ~ None            => ConstructorDef(Id(name), Nil)
    }

  lazy val SET_ARGS: PParser[List[(String, String)]] =
    (VARIABLE <~ ":") ~ (SET | VARIABLE) ~ ("," ~> SET_ARGS).? ^^ {
      case v1 ~ v1t ~ Some(rest) => (v1, v1t) :: rest
      case v1 ~ v1t ~ None       => List((v1, v1t))
    }

  lazy val ATOM: PParser[Term] = LIST | BIT | INTEGER | FLOAT | ID | (SET ^^ SetId)

  lazy val PRGM: PParser[List[Term]] = (LINE | EMPTYLINE).*

  lazy val EMPTYLINE = (EOL | " ") ^^ { _ =>
    Empty
  }

  lazy val EOF: PParser[String] = """\z""".r ^^ identity
  lazy val EOL: PParser[String] = (sys.props("line.separator") | "\r\n" | "\\n".r) ^^ identity

  lazy val LINE: PParser[Term] = (SET_DEF | NAMED | TERM) <~ (EOL | EOF)

  lazy val NAMED: PParser[Named] =
    ID ~ ("=" ~> TERM) ^^ { case id ~ body => Named(id, body) }

  lazy val TERM: PParser[Term] = LIST | LABSTR | APP | ATOM | PEXPR

  lazy val PEXPR: PParser[Term] = "(" ~> TERM <~ ")"

  lazy val ID: PParser[Id] =
    VARIABLE ^^ Id

  lazy val LABSTR: PParser[Abstr] =
    TERM ~ "." ~ TERM ^^ { case id ~ _ ~ term => Abstr(id, term) }

  lazy val APP: PParser[Term] =
    TERM ~ (PEXPR | ATOM) ^^ { case t1 ~ t2 => Applic(t1, t2) }

}

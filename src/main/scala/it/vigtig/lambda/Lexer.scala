package it.vigtig.lambda

import scala.util.parsing.combinator.{PackratParsers, RegexParsers}



trait Lexer {
  def tokens: Either[String, List[Token]]
}

object Lexer {

  def main(args: Array[String]) {

    val program =
      """
      |x = 20
      |* ((a . b . + a b) x 1) 2
    """.stripMargin

    println(program)
    println(FraseLexer(program).tokens)

  }

}

case class FraseLexer(program: String) extends Lexer with RegexParsers with PackratParsers {

  private def tokenParser: PackratParser[List[Token]] =
    phrase(
      rep1(
          `true` |
          `false` |
          float |
          integer |
          period |
          plus |
          minus |
          division |
          equals |
          comma |
          space |
          newline |
          leftParen |
          rightParen |
          identifier))

  def tokens = parse(tokenParser, program) match {
    case Success(tokens, _) => Right(tokens)
    case NoSuccess(result, _) => Left(result)
  }

  lazy val float: PackratParser[FLOAT]         = """-?(\d+(\.\d*)?|\d*\.\d+)([eE][+-]?\d+)?[fFdD]?""".r ^^ FLOAT
  lazy val integer: PackratParser[INTEGER]     = """-?\d+""".r ^^ INTEGER
  lazy val variable: PackratParser[IDENTIFIER] = """<=|(?!or)(?!set)[a-z+\-\\/\*=%<][a-zA-Z]*""".r ^^ IDENTIFIER

  lazy val `false`: PackratParser[FALSE.type] = "false" ^^ { _ =>
    FALSE
  }
  lazy val `true`: PackratParser[TRUE.type] = "true" ^^ { _ =>
    TRUE
  }


  lazy val identifier: PackratParser[IDENTIFIER] = """<=|(?!or)(?!set)[a-z+\-\\/\*=%<][a-zA-Z]*""".r ^^ IDENTIFIER

  // format: off
  lazy val leftParen: PackratParser[`(`.type]  = "(" ^^ {_ => `(`}
  lazy val rightParen: PackratParser[`)`.type] = ")" ^^ {_ => `)`}
  lazy val period: PackratParser[`.`.type]     = "." ^^ { _ => `.` }
  lazy val plus: PackratParser[`+`.type]       = "+" ^^ { _ => `+` }
  lazy val minus: PackratParser[`-`.type]      = "-" ^^ { _ => `-` }
  lazy val division: PackratParser[`/`.type]   = "/" ^^ { _ =>`/` }
  lazy val equals: PackratParser[`=`.type]     = "=" ^^ {_ => `=`}

  lazy val comma: PackratParser[COMMA.type] = "," ^^ { _ => COMMA }
  lazy val space: PackratParser[SPACE.type] = " " ^^ { _ => SPACE }

  // format: on
  lazy val newline: PackratParser[NEWLINE.type] = (sys.props("line.separator") | "\r\n" | "\\n".r) ^^ { _ =>
    NEWLINE
  }


}

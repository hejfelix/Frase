package com.lambdaminute.grammar

import com.lambdaminute.errors.LexerError

import scala.util.parsing.combinator.{PackratParsers, RegexParsers}

case class DefaultLexer() extends Lexer with RegexParsers with PackratParsers {

  override val whiteSpace = """[ ]+""".r

  private def tokenParser: PackratParser[List[Token]] =
    phrase(
      rep1(
        `true` |
          `false` |
          float |
          integer |
          period |
          equals |
          comma |
          space |
          newline |
          leftParen |
          rightParen |
          identifier))

  def tokenize(program: String): Either[LexerError[Input], List[Token]] = parse(tokenParser, program) match {
    case Success(tokens, _)             => Right(tokens)
    case NoSuccess(result, next: Input) => Left(LexerError(result, next))
  }

  lazy val float: PackratParser[FLOAT]   = """-?(\d+(\.\d*)|\d*\.\d+)""".r ^^ FLOAT
  lazy val integer: PackratParser[INTGR] = """-?\d+""".r ^^ INTGR
  lazy val variable: PackratParser[ID]   = """<=|(?!or)(?!set)[a-z+\-\\/\*=%<][a-zA-Z]*""".r ^^ ID

  lazy val `false`: PackratParser[FALSE.type] = "false" ^^ { _ =>
    FALSE
  }
  lazy val `true`: PackratParser[TRUE.type] = "true" ^^ { _ =>
    TRUE
  }

  lazy val identifier: PackratParser[ID] = """<=|(?!or)(?!set)[a-z+\-\\/\*=%<][a-zA-Z]*""".r ^^ ID

  // format: off
  lazy val leftParen: PackratParser[`(`.type]  = "(" ^^ {_ => `(`}
  lazy val rightParen: PackratParser[`)`.type] = ")" ^^ {_ => `)`}
  lazy val period: PackratParser[`.`.type]     = "." ^^ { _ => `.` }
  lazy val equals: PackratParser[`=`.type]     = "=" ^^ {_ => `=`}
  lazy val comma: PackratParser[`,`.type]      = "," ^^{ _ => `,` }
  lazy val space: PackratParser[SPACE.type]    = " " ^^{ _ => SPACE }
  // format: on

  lazy val newline: PackratParser[NEWLINE.type] = (sys.props("line.separator") | "\r\n" | "\\n".r) ^^ { _ =>
    NEWLINE
  }

}

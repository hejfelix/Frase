import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.PackratParsers
import scala.language.postfixOps

class LoopParser extends RegexParsers with PackratParsers {

  lazy val identifier  = """[_\p{L}][_\p{L}\p{Nd}]*""".r

  lazy val integer:PackratParser[Int]     = """(0|[1-9]\d*)""".r ^^ { _.toInt }

  lazy val loop:PackratParser[Statement] =
      "for" ~ identifier ~ "in" ~ integer ~ "to" ~ integer ~ statement ^^
        {
          case f~variable~i~lBound~t~uBound~statement =>
            ForLoop(variable, lBound, uBound,statement)
        }

  lazy val statements = statement*

  lazy val block = "{"~>statements<~"}"  ^^ { l => Block(l) }

  lazy val statement : PackratParser[Statement] = loop | block

}

abstract trait Statement
case class Block(statements : List[Statement]) extends Statement
case class ForLoop(variable: String, lowerBound:Int, upperBound: Int, statement:Statement) extends Statement

object TestLoopParser extends LoopParser with App {
  parseAll(loop, "for x in 1 to 42 { for y in 0 to 1 {} }") match {
    case Success(lup,_) => println(lup)
    case x => println(x)
  }
}

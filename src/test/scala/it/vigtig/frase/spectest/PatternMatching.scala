package it.vigtig.frase.spectest

import it.vigtig.lambda.{InterpreterLike, ParserLike}
import org.scalatest.PropSpec
import org.scalatest.prop.GeneratorDrivenPropertyChecks

import org.scalatest.Matchers.convertToAnyShouldWrapper

/**
 * Created by Hargreaves on 05/09/15.
 */
class PatternMatching extends PropSpec
with InterpreterLike
with ParserLike
with ASTGenerators
with GeneratorDrivenPropertyChecks {

  def parseProgramTest(l: String)(b: Term => Unit) = {
    interpretProgram(l) match {
      case Some(ts) => ts foreach (t => b(interpret(t)()))
      case None     => ???
    }
  }

  val LIST =
    """
set List = Nil or Cons x:Int,xs:List
head = (Cons x xs) . x
tail = (Cons x xs) . xs
head (Cons 42 Nil)
    """

  property("Head") {
     parseAll(PRGM,LIST) match {
       case Success(r,_) => println(r.mkString("\n"))
       case _ =>
     }
  }

}

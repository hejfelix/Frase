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
      case Some(ts) => ts.filter(_!=Empty) foreach (t => b(interpret(t)()))
      case None     => ???
    }
  }

  val LIST =
    """
set List = Nil or Cons x:Int,xs:List

head = (Cons x xs) . x
tail = (Cons x xs) . xs

size = Nil . 0
size = (Cons x xs) . (+ 1 (size xs))
    """

  property("Head") {
    parseProgramTest(LIST+s"\nhead (Cons 42 Nil)")(_ shouldBe Integer(42))
  }

  property("Tail") {
    parseProgramTest(LIST+s"\ntail (Cons 42 Nil)")(_ shouldBe SetId("Nil"))
  }


  property("size Nil should be 0") {
    parseProgramTest(LIST+s"\nsize Nil")(_ shouldBe Integer(0))
  }


  property("Size") {
    parseProgramTest(LIST+s"\nsize (Cons 42 (Cons 1337 (Cons 9000 Nil)))")(_ shouldBe Integer(3))
  }


}

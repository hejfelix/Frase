/**
 * @author Felix
 */
package it.vigtig.frase.spectest

import it.vigtig.lambda.OldParserLike
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Matchers, PropSpec}

//class FraseGenerator extends PropSpec
//with DefaultInterpreter
//with Matchers
//with OldParserLike
//with ASTGenerators
//with GeneratorDrivenPropertyChecks {
//
//  property("Ids are leaves") {
//    forAll {
//      (id: Id) =>
//        id should matchPattern { case Id(_) => }
//    }
//  }
//
//  property("Atoms cannot be reduced") {
//    forAll {
//      (a: Atom) =>
//        interpret(a)() shouldEqual a
//    }
//  }
//
//  def parseLineTest(l: String)(b: Term => Unit) {
//    parseAll(LINE, l) match {
//      case Success(ast, _) => b(interpret(ast)())
//      case pr => fail("Term failed to parse" + pr)
//    }
//  }
//
//  property("Constant-function reduces to constant") {
//    forAll {
//      (x: String, y: String) =>
//        whenever(x != y) {
//          parseLineTest(s"($x . $y) $x")(_ shouldBe Id(y))
//        }
//    }
//  }
//
//  property("Identity-function reduces to identity") {
//    forAll {
//      (x: String, y: String) =>
//        whenever(x != y && x.length > 0 && y.length > 0) {
//          parseLineTest(s"($x . $x) $y")(_ shouldBe Id(y))
//        }
//    }
//  }
//
//  property("'false' returns 2nd argument") {
//    forAll {
//      (x: String, y: String) =>
//        whenever(x != y) {
//          parseLineTest(s"false $x $y")(_ shouldBe Id(y))
//        }
//    }
//  }
//
//  property("'true' returns 1st argument") {
//    forAll {
//      (x: String, y: String) =>
//        whenever(x != y) {
//          parseLineTest(s"true $x $y")(_ shouldBe Id(x))
//        }
//    }
//  }
//
//
//}
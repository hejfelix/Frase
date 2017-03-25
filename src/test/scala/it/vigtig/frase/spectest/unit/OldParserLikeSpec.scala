package it.vigtig.frase.spectest.unit

import it.vigtig.lambda.OldParserLike
import org.scalatest.{FlatSpec, Matchers}

class OldParserLikeSpec
  extends FlatSpec
    with Matchers
    with OldParserLike {

  behavior of "Parser combinator grammar"

  it should "be able to parse sugar lists" in {
    parseAll(PRGM,"[1,2,3]") match {
      case Success(terms,_) =>
        terms should contain (Applic(Applic(SetId("Cons"),Integer(1)),Applic(Applic(SetId("Cons"),Integer(2)),Applic
                                                  (Applic(SetId("Cons"),Integer(3)),SetId("Nil")))))
      case _ => fail("couldn't parse list")
    }
  }

}

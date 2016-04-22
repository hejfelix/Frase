package it.vigtig.frase.spectest.unit

import it.vigtig.lambda.ParserLike
import org.scalatest.{FlatSpec, Matchers}

class ParserLikeSpec
  extends FlatSpec
    with Matchers
    with ParserLike {

  behavior of "Parser combinator grammar"

  it should "be able to parse sugar lists" in {
    parseAll(PRGM,"[1,2,3]") match {
      case Success(terms,_) =>
        terms should contain (Applic(Applic(SetId("Cons"),Integer(1)),Applic(Applic(SetId("Cons"),Integer(2)),Applic
                                                  (Applic(SetId("Cons"),Integer(3)),SetId("Nil")))))
    }
  }

}

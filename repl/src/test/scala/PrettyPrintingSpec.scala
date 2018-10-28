import com.lambdaminute.frase.syntax.all._
import org.scalatest.{Matchers, WordSpec}

class PrettyPrintingSpec extends WordSpec with Matchers {

  "Term" should {
    "pretty print abstractions" in {
      val prettyTerm = "x . + 42 x"
      prettyTerm.parseUnsafe.pretty shouldBe prettyTerm
    }

    "pretty print applied abstractions" in {
      val prettyTerm = "(x . + 2 x) 2"
      prettyTerm.parseUnsafe.pretty shouldBe prettyTerm
    }
    "pretty print list of applications" in {
      val prettyTerm = "x y z a b c"
      prettyTerm.parseUnsafe.pretty shouldBe prettyTerm
    }

    "pretty print advanced applications" in {
      val prettyTerm = "a (b +) (+ 1 2) 3"
      prettyTerm.parseUnsafe.pretty shouldBe prettyTerm
    }
  }

}

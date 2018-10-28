package com.lambdaminute.frase.calculus.ast

import com.lambdaminute.frase.calculus.math._
import com.lambdaminute.frase.calculus.pretty.PrettyPrinter

object Ast {
  object TerminalColors {
    val Green: String = Console.GREEN
    val Off: String   = Console.RESET
  }

  object Syntax {
    implicit class AstSyntax(self: Term) {
      def app(that: Term) = Application(self, that)
      def abs(that: Term) = LambdaAbstraction(self, that)
    }
  }

  sealed trait Term {

    import TerminalColors._
    def prettyType: String = this match {
      case Bool(b)                  => b.toString
      case Identifier(id)           => s"$Green$id$Off"
      case Integer(i)               => i.toString
      case Floating(f)              => f.toString
      case Application(left, right) => s"(${left.prettyType} ${right.prettyType})"
      case LambdaAbstraction(id @ Identifier(_), body @ Identifier(_)) =>
        s"${id.prettyType} -> ${body.prettyType}"
      case LambdaAbstraction(id, body @ Identifier(_)) =>
        s"(${id.prettyType}) -> ${body.prettyType}"
      case LambdaAbstraction(id, body) => s"(${id.prettyType}) -> (${body.prettyType})"
    }

    def pretty: String = PrettyPrinter.apply.pretty(this)

    def contains(term: Term): Boolean =
      this match {
        case `term`                      => true
        case Application(lhs, rhs)       => lhs.contains(term) || rhs.contains(term)
        case LambdaAbstraction(id, body) => id.contains(term) || body.contains(term)
        case _                           => false
      }

    def isApplication = this match {
      case _: Application => true
      case _              => false
    }

    def isAtom: Boolean = this match {
      case Bool(_) | Floating(_) | Integer(_) | Identifier(_) => true
      case _                                                  => false
    }

    def transform(f: PartialFunction[Term, Term]): Term = {

      val fallback: PartialFunction[Term, Term] = {
        case Application(left, right)    => Application(left.transform(f), right.transform(f))
        case LambdaAbstraction(id, body) => LambdaAbstraction(id.transform(f), body.transform(f))
        case x                           => x
      }
      (f orElse fallback)(this)
    }

    def enumerate: List[Term] = this match {
      case Application(left, right)    => (this :: left.enumerate) ::: right.enumerate
      case LambdaAbstraction(id, body) => this :: id :: body.enumerate
      case _                           => this :: Nil
    }

    def size: Int = enumerate.size

    /**
      * The set of free variables in t (=variables that have not been bound)
      *
      * @return The set containing all free variables inside `this`
      */
    def freeVars: Set[Identifier] = this match {
      case a @ Identifier(_)                       => Set(a)
      case LambdaAbstraction(id: Identifier, body) => body.freeVars - id
      case Application(a, b)                       => a.freeVars ++ b.freeVars
      case _                                       => Set()
    }

    //Capture-avoiding substitution
    def substitute(label: (Term, Term)): Term = (this, label) match {
      case (i: Identifier, (j, k)) if i == j => k
      case (i: Identifier, _)                => i
      case (Application(a, b), _) =>
        Application(a.substitute(label), b.substitute(label))
      case (LambdaAbstraction(id: Identifier, body), (oldLabel, newLabel))
          if id != oldLabel && !newLabel.freeVars(id) =>
        LambdaAbstraction(id, body.substitute(label))
      case (a @ LambdaAbstraction(_, _), _) => a
      case _                                => this
    }

    def nextAvailableId: Identifier = nextAvailableId(Set.empty)

    def nextAvailableId(ignoreKeywords: Set[String]): Identifier = {
      val filteredVars = vars.map(_.id).filter(!ignoreKeywords.contains(_))
      if (filteredVars.isEmpty)
        Identifier("a")
      else
        Identifier(increment(filteredVars.max))
    }

    def vars: List[Identifier] = this.enumerate.collect { case i @ Identifier(_) => i }

    private def increment(s: String): String =
      (s.map(_ - 'a').reverse.fromBase(base = 26) + 1)
        .toBase(base = 26)
        .reverse
        .map(_ + 'a')
        .map(_.toChar)
        .mkString

    def unshadow: Term = this match {
      case LambdaAbstraction(arg: Identifier, body) if !body.freeVars.contains(arg) =>
        val newId = nextAvailableId
        LambdaAbstraction(newId, body.substitute(arg -> newId).unshadow)
      case LambdaAbstraction(arg, body) =>
        LambdaAbstraction(arg, body.unshadow)
      case _ => this
    }

    def relabel(oldLabel: Term, newLabel: Term): Term =
      transform {
        case `oldLabel` => newLabel
      }

    def isMono(s: String): Boolean = !s.isEmpty && s.head.isUpper
    def isPoly(s: String): Boolean = !isMono(s)

  }

  case class Bool(b: Boolean)                        extends Term
  case class Identifier(id: String)                  extends Term
  case class Floating(f: Float)                      extends Term
  case class Integer(i: Int)                         extends Term
  case class Application(left: Term, right: Term)    extends Term
  case class LambdaAbstraction(id: Term, body: Term) extends Term

}

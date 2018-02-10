package com.lambdaminute.frase.calculus.ast

import com.lambdaminute.frase.calculus.math._

object AST {
  object TerminalColors {
    val Green: String = Console.GREEN
    val Off: String   = Console.RESET
  }

  sealed trait Fragment {

    import TerminalColors._
    def prettyType: String = this match {
      case Named(lhs, rhs)                                             => s"${lhs.prettyType} = ${rhs.prettyType}"
      case Bool(b)                                                     => b.toString
      case Identifier(id)                                              => s"$Green$id$Off"
      case Integer(i)                                                  => i.toString
      case Floating(f)                                                 => f.toString
      case Application(left, right)                                    => s"(${left.prettyType} ${right.prettyType})"
      case LambdaAbstraction(id @ Identifier(_), body @ Identifier(_)) => s"${id.prettyType} -> ${body.prettyType}"
      case LambdaAbstraction(id, body @ Identifier(_))                 => s"(${id.prettyType}) -> ${body.prettyType}"
      case LambdaAbstraction(id, body)                                 => s"(${id.prettyType}) -> (${body.prettyType})"
      case Empty                                                       => ""
    }

    def pretty: String = this match {
      case Named(lhs, rhs)             => s"${lhs.pretty} = ${rhs.pretty}"
      case Bool(b)                     => b.toString
      case Identifier(id)              => id
      case Integer(i)                  => i.toString
      case Floating(f)                 => f.toString
      case Application(left, right)    => s"(${left.pretty} ${right.pretty})"
      case LambdaAbstraction(id, body) => s"${id.pretty} . (${body.pretty})"
      case Empty                       => ""
    }

    def contains(term: Term): Boolean =
      this match {
        case `term`                      => true
        case Named(lhs, rhs)             => lhs.contains(term) || rhs.contains(term)
        case Application(lhs, rhs)       => lhs.contains(term) || rhs.contains(term)
        case LambdaAbstraction(id, body) => id.contains(term) || body.contains(term)
        case _                           => false
      }

  }

  sealed trait Declaration extends Fragment {}
  sealed trait Term extends Fragment {

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

    def size = enumerate.size

    /**
      * The set of free variables in t (=variables that have not been bound)
      * @return The set containing all free variables inside `this`
      */
    def freeVars: Set[Identifier] = this match {
      case a @ Identifier(_)                       => Set(a)
      case LambdaAbstraction(id: Identifier, body) => body.freeVars - id
      case Application(a, b)                       => a.freeVars ++ b.freeVars
      case Empty                                   => Set()
      case _                                       => Set()
    }

    //Capture-avoiding substitution
    def substitute(label: (Term, Term)): Term = (this, label) match {
      case (Empty, _)                        => Empty
      case (i: Identifier, (j, k)) if i == j => k
      case (i: Identifier, _)                => i
      case (Application(a, b), _)            => Application(a.substitute(label), b.substitute(label))
      case (LambdaAbstraction(id: Identifier, body), (x, y)) if id != x && !y.freeVars(id) =>
        LambdaAbstraction(id, body.substitute(label))
      case (a @ LambdaAbstraction(_, _), _) => a
      case _                                => this
    }

    def nextAvailableId: Identifier = Identifier(increment(this.enumerate.collect { case Identifier(x) => x }.max))

    def vars: List[Identifier] = this.enumerate.collect { case Identifier(x) => Identifier(x) }

    private def increment(s: String): String =
      (s.map(_ - 'a').reverse.fromBase(26) + 1).toBase(26).reverse.map(_ + 'a').map(_.toChar).mkString

    def unshadow: Term = this match {
      case LambdaAbstraction(arg: Identifier, body) if !body.freeVars.contains(arg) =>
        val newId = nextAvailableId
        println(s"$arg is not free in ${body.pretty}")
        LambdaAbstraction(newId, body.substitute(arg -> newId).unshadow)
      case LambdaAbstraction(arg, body) =>
        LambdaAbstraction(arg, body.unshadow)
      case _ => this
    }

    def relabel(oldLabel: Term, newLabel: Term): Term =
      transform {
        case `oldLabel` => newLabel
      }

    def isMono(s: String) = !s.isEmpty && s.head.isUpper
    def isPoly(s: String) = !isMono(s)



  }

  case class Named(lhs: Identifier, rhs: Term) extends Declaration

  case class Bool(b: Boolean)                        extends Term
  case class Identifier(id: String)                  extends Term
  case class Floating(f: Float)                      extends Term
  case class Integer(i: Int)                         extends Term
  case class Application(left: Term, right: Term)    extends Term
  case class LambdaAbstraction(id: Term, body: Term) extends Term

  case object Empty extends Term
}

package com.lambdaminute.syntax

object AST {
  object TerminalColors {
    val Green = Console.GREEN
    val Off   = Console.RESET
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

    def relabel(oldLabel: Identifier, newLabel: Identifier): Term =
      transform {
        case `oldLabel` => newLabel
      }

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

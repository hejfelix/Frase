package com.lambdaminute.ast

import cats.implicits._
import com.lambdaminute.errors.{FraseError, UnificationError}
import com.lambdaminute.math._

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

    private def combineUnifications(a: Map[Term, Term], b: Map[Term, Term]): Either[FraseError, Map[Term, Term]] = {

      val commonKeys: List[Term] = a.keys.filter(b.contains).toList

      val maybeEquations: Either[FraseError, List[Map[Term, Term]]] = commonKeys
        .traverse { key: Term =>
          a(key).unifyBlindly(b(key))
        }

      val maybeSingleEquation: Either[FraseError, Map[Term, Term]] = maybeEquations
        .map(_.fold(Map.empty)(_ ++ _))

      maybeSingleEquation
        .map(_ ++ a ++ b)
    }

    def unify(that: Term): Either[FraseError, Map[Term, Term]] =
      this
        .unifyBlindly(that)
        .map(_.filter {
          case (x, y) if x != y => true
          case _                => false
        })

    def isMono(s: String) = !s.isEmpty && s.head.isUpper
    def isPoly(s: String) = !isMono(s)

    //TODO: fix with this https://en.wikipedia.org/wiki/Unification_(computer_science)
    private def unifyBlindly(that: Term): Either[FraseError, Map[Term, Term]] = (this, that) match {
      case (Bool(x), Bool(y)) if x == y             => Right(Map.empty)
      case (Identifier(x), y) if isPoly(x)          => Right(Map(Identifier(x) -> y))
      case (y, Identifier(x)) if isPoly(x)          => Right(Map(Identifier(x) -> y))
      case (Identifier(x), Identifier(y)) if x == y => Right(Map.empty)
      case (Floating(x), Floating(y)) if x == y     => Right(Map.empty)
      case (Integer(x), Integer(y)) if x == y       => Right(Map.empty)
      case (Empty, Empty)                           => Right(Map.empty)
      case (Application(l1, r1), Application(l2, r2)) =>
        for {
          leftResult  <- l1.unifyBlindly(l2)
          rightResult <- r1.unifyBlindly(r2)
          combined    <- combineUnifications(leftResult, rightResult)
        } yield combined

      case (LambdaAbstraction(argl, bodyl), LambdaAbstraction(argr, bodyr)) =>
        for {
          argResult  <- argl.unifyBlindly(argr)
          bodyResult <- bodyl.unifyBlindly(bodyr)
          combined   <- combineUnifications(argResult, bodyResult)
        } yield combined
      case _ => Left(UnificationError(s"Unable to unify ${this.pretty} with ${that.pretty}"))
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

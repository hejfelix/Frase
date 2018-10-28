package com.lambdaminute.frase.calculus.pretty
import cats.Monoid
import cats.implicits._
import com.lambdaminute.frase.calculus.ast.Ast._

import scala.annotation.tailrec

object PrettyPrinter {

  def apply = new PrettyPrinter

  implicit val partialFunctionMonoid: Monoid[PartialFunction[Term, String]] =
    new Monoid[PartialFunction[Term, String]] {
      override def empty: PartialFunction[Term, String] =
        new PartialFunction[Term, String] {
          override def isDefinedAt(x: Term): Boolean = false
          override def apply(v1: Term): String       = ???
        }
      override def combine(x: PartialFunction[Term, String],
                           y: PartialFunction[Term, String]): PartialFunction[Term, String] =
        x orElse y
    }
}

class PrettyPrinter {
  import PrettyPrinter.partialFunctionMonoid

  lazy val pretty: PartialFunction[Term, String] =
    List(sequenceOfApplications, appliedAbstraction, singleAbstraction, fallback).combineAll

  private lazy val singleAbstraction: PartialFunction[Term, String] = {
    case LambdaAbstraction(id, body) => s"${pretty(id)} . ${pretty(body)}"
  }

  private lazy val appliedAbstraction: PartialFunction[Term, String] = {
    case Application(l, r) => s"(${pretty(l)}) ${pretty(r)}"
  }

  private lazy val sequenceOfApplications: PartialFunction[Term, String] = {
    case Application(l, r) if r.isApplication             => s"${pretty(l)} (${pretty(r)})" // right association
    case Application(l, r) if l.isAtom || l.isApplication => s"${pretty(l)} ${pretty(r)}" // left association by default
  }

  @tailrec
  private def isSequenceOfApplications(t: Term): Boolean =
    t match {
      case Application(l, r) if l.isAtom => isSequenceOfApplications(r)
      case _ if t.isAtom                 => true
      case _                             => false
    }

  private lazy val fallback: PartialFunction[Term, String] = {
    case Bool(b)                     => b.toString
    case Identifier(id)              => id
    case Integer(i)                  => i.toString
    case Floating(f)                 => f.toString
    case Application(left, right)    => s"${pretty(left)} ${pretty(right)}"
    case LambdaAbstraction(id, body) => s"${pretty(id)} . ${pretty(body)}"
  }

}

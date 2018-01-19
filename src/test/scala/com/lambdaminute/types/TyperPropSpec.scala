package com.lambdaminute.types

import com.lambdaminute.syntax.AST
import com.lambdaminute.syntax.AST._
import org.scalacheck.{Arbitrary, Gen}

trait TermGenerators {

  def termGen: Gen[AST.Term] =
    Gen.oneOf(leafs, nodes)

  def nodes: Gen[AST.Term] = Gen.oneOf(appGen, lambdaGen)

  def leafs =
    Gen.oneOf(idGen, floatingGen, boolGen)

  def appGen: Gen[Application] =
    for {
      _     <- leafs // Getting stack overflow when I have a recursive call here :S
      left  <- termGen
      right <- termGen
    } yield Application(left, right)

  def lambdaGen: Gen[LambdaAbstraction] =
    for {
      id   <- idGen
      body <- termGen
    } yield LambdaAbstraction(id, body)

  def idGen: Gen[Identifier]     = Gen.alphaLowerChar.map(_.toString).map(Identifier.apply)
  def floatingGen: Gen[Floating] = Arbitrary.arbFloat.arbitrary.map(Floating.apply)
  def boolGen: Gen[Bool]         = Arbitrary.arbBool.arbitrary.map(Bool.apply)

  implicit val arbApp: Arbitrary[Application] = Arbitrary { appGen }
  implicit val arbTerm: Arbitrary[Term]       = Arbitrary { termGen }
  implicit val arbId: Arbitrary[Identifier]   = Arbitrary { idGen }
  implicit val argFloat: Arbitrary[Any]       = Arbitrary { floatingGen }
  implicit val arbBool: Arbitrary[Bool]       = Arbitrary { boolGen }

}

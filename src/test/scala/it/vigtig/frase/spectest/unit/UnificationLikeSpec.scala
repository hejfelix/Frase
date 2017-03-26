package it.vigtig.frase.spectest.unit

import it.vigtig.lambda.ASTLike
import org.scalatest.{FlatSpec, Matchers}
//
//class UnificationLikeSpec
//  extends FlatSpec
//  with Matchers
//  with UnificationLike
//  with ASTLike {
//
//  behavior of "Unification algorithm"
//
//  it should "unify 2 IDs with the same names" in {
//    unify(Id("x"),Id("x")) should contain (Map.empty)
//  }
//
//  it should "unify anything with a variable on righthand side" in {
//    unify(Integer(42),Id("x")) should contain  (Map( Id("x") -> Integer(42)))
//  }
//
//  it should "unify 2 abstractions" in {
//
//    val x = Id("x1")
//    val y = Integer(42)
//
//    val a = Floating(1337.0f)
//    val b = Id("x2")
//
//    val left = Abstr(x,y)
//    val right = Abstr(a,b)
//
//    val unified = unify(left,right)
//
//    unified should contain (Map(x -> a,b -> y))
//
//  }
//
//  it should "unify 2 lists with same length" in {
//    val terms:List[Term] = List(Id("x"),Id("y"))
//    unifyLists(terms,terms) should contain (Map.empty)
//  }
//
//  it should "not unify lists with different sizes" in {
//    unifyLists(List(Id("x")),Nil) shouldBe empty
//  }
//
//  it should "get the header of a term" in {
//    val named:Term = Named(Id("_"), Abstr(Id("x"), Abstr(Id("y"), Integer(42))))
//    val setId:Term = Applic(SetId("S"),Abstr(Id("x"),Integer(42)))
//
//    header(named) shouldBe List(Id("x"),Id("y"))
//    header(setId) should contain allOf (SetId("S"),Id("x"))
//    header(Id("x")) shouldBe empty
//  }
//
//
//
//}

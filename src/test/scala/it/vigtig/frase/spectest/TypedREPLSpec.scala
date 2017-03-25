package it.vigtig.frase.spectest

import java.io.ByteArrayInputStream
//
//import it.vigtig.lambda.TypedREPL
//import it.vigtig.lambda.TypedREPL.InvalidProgramException
//import org.scalatest.{FlatSpec, Matchers}
//
//class TypedREPLSpec
//  extends FlatSpec
//    with
//    Matchers {
//
//  behavior of "REPL"
//
//  it should "parse a simple program" in
//  {
//    val testProgram =
//      "fib = n . (<= n 2) (1) ((+ (fib (- n 2)) (fib (- n 1))))" +
//      "\nset List a = Nil or Cons head:a,tail:List" +
//      "\nfac = 0 . 1" +
//      "\nfac = n . * n (fac (- n 1))" +
//      "\n:exit\n"
//
//
//    val in = new ByteArrayInputStream(testProgram.getBytes)
//    Console.withIn(in)
//    {
//      try
//      {
//        TypedREPL.main(Array.empty)
//        1 shouldBe 1
//      } catch
//      {
//        case InvalidProgramException(msg) =>
//      }
//    }
//
//  }
//
//  it should "throw InvalidProgramException on syntax errors" in
//  {
//    val testProgram =
//      ".."
//
//    val in = new ByteArrayInputStream(testProgram.getBytes)
//    Console.withIn(in)
//    {
//      intercept[InvalidProgramException]
//        {
//          TypedREPL.main(Array.empty)
//        }
//    }
//  }
//
//}

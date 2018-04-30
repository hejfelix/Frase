package com.lambdaminute.frase.lang.types

import cats.effect.IO
import com.lambdaminute.frase.calculus.ast.AST.Integer
import com.lambdaminute.frase.calculus.grammar.{DefaultLexer, DefaultParser}
import com.lambdaminute.frase.calculus.interpreter.{ConcurrentInterpreter, DefaultBuiltins, DefaultInterpreter}
import com.lambdaminute.frase.calculus.semantic.DefaultKeywords
import org.scalatest.{EitherValues, Matchers, WordSpec}

import concurrent.duration._
import scala.concurrent.Await

class ConcurrentInterpreterSpec extends WordSpec with Matchers with EitherValues {

  "ConcurrentInterpreterSpec" should {
    "work" in {
      val interp =
        new ConcurrentInterpreter[IO](DefaultParser(DefaultLexer()), DefaultKeywords(), DefaultBuiltins.builtIns)
      interp.interpret("+ 1 2").value.unsafeRunSync.right.value shouldBe Integer(3)
    }

    def time[T](b: => T): (Duration, T) = {
      val start  = System.currentTimeMillis()
      val result = b
      ((System.currentTimeMillis() - start).millis, result)
    }

    "interpret fibonacci function" in {

      def fib = "yCombinator (fib . n . ( (<= n 1) n (+ (fib (- n 2)) (fib (- n 1))))) 24"

      val parser = DefaultParser(DefaultLexer())

      val concurrentInterpreter =
        new ConcurrentInterpreter[IO](parser, DefaultKeywords(), DefaultBuiltins.builtIns)

      val sequentialInterpreter =
        DefaultInterpreter(parser, DefaultKeywords(), DefaultBuiltins.builtIns)

      val io                 = concurrentInterpreter.interpret(fib).value
      val timeLimit          = 10.seconds
      val (concTime, result) = time { Await.result(io.unsafeToFuture(), timeLimit).right.get }

      val (seqTime, seqResult) = time { sequentialInterpreter.interpret(fib).right.get }

      println(s"Concurrent time: $concTime, Sequential time: ${seqTime}")
      result shouldBe seqResult
    }
  }

}

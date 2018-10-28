package com.lambdaminute.frase.lang.types

import cats.effect.IO
import com.lambdaminute.frase.calculus.ast.Ast.Integer
import com.lambdaminute.frase.calculus.grammar.{DefaultLexer, DefaultParser}
import com.lambdaminute.frase.calculus.interpreter.{ConcurrentInterpreter, DefaultBuiltins, DefaultInterpreter}
import com.lambdaminute.frase.calculus.semantic.DefaultKeywords
import org.scalatest.{AsyncWordSpec, EitherValues, Matchers}

import scala.concurrent.Future
import scala.concurrent.duration._

class ConcurrentInterpreterSpec extends AsyncWordSpec with Matchers with EitherValues {

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
    def timeAsync[T](b: => Future[T]): Future[(Duration, T)] = {
      val start = System.currentTimeMillis()
      b.map(r => ((System.currentTimeMillis() - start).millis, r))
    }

    "interpret fibonacci function" in {

      def fib = "yCombinator (fib . n . ( (<= n 1) n (+ (fib (- n 2)) (fib (- n 1))))) 17"

      val parser = DefaultParser(DefaultLexer())

      val concurrentInterpreter =
        new ConcurrentInterpreter[IO](parser, DefaultKeywords(), DefaultBuiltins.builtIns)

      val sequentialInterpreter =
        DefaultInterpreter(parser, DefaultKeywords(), DefaultBuiltins.builtIns)

      val io = concurrentInterpreter.interpret(fib).value

      for {
        (concTime, result)   <- timeAsync { io.unsafeToFuture() }
        (seqTime, seqResult) <- Future(time { sequentialInterpreter.interpret(fib).right.get })
      } yield {
        println(s"Concurrent time: $concTime, Sequential time: $seqTime")
        result.right.value shouldBe seqResult
      }
    }
  }

}

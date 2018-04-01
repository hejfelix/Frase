package com.lambdaminute.frase.calculus

package object debug {
  def trace[T](x: T): T = { println(x); x }
  def trace[T,U](f: T => U)(x: T): T = { println(f(x)); x }
}

package com.lambdaminute.frase.calculus.interpreter

trait FixPoint {

  def fixPoint[T](t: T)(p: T => T): T = {
    val pOfT = p(t)
    if (t != pOfT)
      fixPoint(pOfT)(p)
    else
      pOfT
  }
}

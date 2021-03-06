package com.lambdaminute.frase.calculus

package object math {

  implicit class BaseMathInt(i: Int) {
    def toBase(base: Int): List[Int] = if (i < base) List(i) else i % base :: (i / base).toBase(base)
  }

  implicit class BaseMathSeq(xs: Seq[Int]) {
    def fromBase(base: Int): Int =
      xs.zipWithIndex.map { case (b, i) => scala.math.pow(base.toDouble, i.toDouble).toInt * b }.sum
  }

}

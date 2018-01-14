package com.lambdaminute

package object math {

  implicit class BaseMathInt(i: Int) {
    def toBase(base: Int)                          = toBaseHelper(base)
    private def toBaseHelper(base: Int): List[Int] = if (i < base) List(i) else i % base :: (i / base).toBase(base)
  }

  implicit class BaseMathSeqList(xs: Seq[Int]) {
    def fromBase(base: Int): Int =
      xs.zipWithIndex.map { case (b, i) => scala.math.pow(base.toDouble, i.toDouble).toInt * b }.sum
  }

}

package com.avaglir.util.algebra.test

import com.avaglir.util.algebra._
import com.avaglir.util.algebra.abstrct.{Field, IntegralDomain}
import utest._
import utest.framework.{Test, Tree}

object FieldTest extends TestSuite {
  val tests: Tree[Test] = this {
    'FieldForNumeric {
      val x: Int = 4
      val domain = implicitly[IntegralDomain[Int]]
      domain.add(x, 12) ==> 16
      domain.inverse(x) ==> -4
      domain.mult(3, x) ==> 12

      val y: Float = 4f
      val field = implicitly[Field[Float]]
      field.add(y, 32) ==> y+32
      field.mult(y, 18) ==> y*18
      field.div(y, 2f) ==> y/2f
      intercept[ArithmeticException] {
        field.div(y, 0)
      }
      field.multiplicativeGroup.inverse(y) ==> 1/y
    }
  }
}

package com.avaglir.util.structure.test

import com.avaglir.util.structure._
import utest._
import utest.framework.{Test, Tree}

object RadixTreeTests extends TestSuite {

  val tests: Tree[Test] = this {
    'BasicStringRadix {
      val strings = Map(
        "a" -> 3,
        "b" -> 4
      )

      val radix = RadixTree(strings)

      radix("a") ==> Some(3)
    }
  }
}

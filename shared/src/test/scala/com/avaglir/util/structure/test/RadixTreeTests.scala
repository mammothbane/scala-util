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

      strings.foreach { case (a, b) => radix(a) ==> b }
      radix.get("c") ==> None
      radix.get("") ==> None
    }

    'ComplexStringRadix {
      val strings = Map(
        "abc" -> 1,
        "bcd" -> 2,
        "abd" -> 3,
        "abe" -> 4,
        "bcde" -> 5,
      )

      val radix = RadixTree(strings)

      strings.foreach { case (a, b) => radix(a) ==> b }
      List("a", "b", "c", "").foreach { radix.get(_) ==> None }

      val radixAMp = radix.children.filterKeys { _ == 'a' }
      radixAMp.size ==> 1

      val radixA = radixAMp.values.head
      radixA.value ==> None
      radixA.children.size ==> 1
      radixA("bc") ==> 1
    }
  }
}

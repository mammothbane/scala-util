package com.avaglir.util.extensions.test

import utest._
import utest.framework.{Test, Tree}

object ExtensionTests extends TestSuite {
  val tests: Tree[Test] = this {
    'GenTraversible {
      'combinations {
        val sample = Array(1, 2, 3, 4)
        assert(sample.combinations(1).flatten.sameElements(sample.iterator))
        (0 until -10).foreach { elt => assert(sample.combinations(elt).flatten.isEmpty) }

        val a = sample.combinations(2).flatten
        assert(a.sameElements(Array(Array(1, 2), Array(1, 3), Array(1, 4), Array(2, 3), Array(2, 4), Array(3, 4)).iterator))
      }
    }
  }
}

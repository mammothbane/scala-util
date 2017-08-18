package com.avaglir.util.extensions.test

import com.avaglir.util.extensions._
import utest._
import utest.framework.{Test, Tree}

object ExtensionTests extends TestSuite {
  def combosEqual[T](a: List[Array[T]], b: List[Array[T]]): Boolean = a.forall { elt => b.exists { _ sameElements elt }} && b.forall { elt => a.exists { _ sameElements elt }}

  val tests: Tree[Test] = this {
    'GenTraversible {
      val sample = Array(1, 2, 3, 4)

      'combinations1 {
        val a = sample.combinations(1).toList
        val cmp = List(Array(1), Array(2), Array(3), Array(4))
        assert(combosEqual(a, cmp))
      }

      'combinations2 {
        val a = sample.combinations(2).toList
        val cmp = List(Array(1, 2), Array(1, 3), Array(1, 4), Array(2, 3), Array(2, 4), Array(3, 4))
        assert(combosEqual(a, cmp))
      }

      'combinations3 {
        val a = sample.combinations(3).toList
        val cmp = List(Array(1, 2, 3), Array(1, 2, 4), Array(1, 3, 4), Array(2, 3, 4))
        assert(combosEqual(a, cmp))
      }

      'combinations4 {
        val a = sample.combinations(4).toList
        val cmp = List(Array(1, 2, 3, 4))
        assert(combosEqual(a, cmp))
      }

      'productEmpty {
        assert(Nil.cartesianProduct(Nil) == Nil)
      }

      'productOne {
        val prod = List(1).cartesianProduct(List(2))
        assert(prod.toList.size == 1)

        val (a, b) = prod.head
        assert(a == 1 && b == 2)
      }

      'productTwo {
        val prod = List(1, 2).cartesianProduct(List(3, 4))
        assert(prod.toList.equals(List((1, 3), (1, 4), (2, 3), (2, 4))))
      }
    }
  }
}

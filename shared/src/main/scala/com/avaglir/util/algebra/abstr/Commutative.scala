package com.avaglir.util.algebra.abstr

/**
  * This is a marker trait. Must be able to prove that the magma operation is commutative.
  * @tparam T
  */
trait Commutative[@specialized(Specializable.AllNumeric) T] {
  this: Magma[T] =>
}

package com.avaglir.util.algebra

/**
  * This is a marker trait. Must be able to prove that the magma operation is commutative.
  * @tparam T
  */
trait Commutative[T] {
  this: Magma[T] =>
}

package com.avaglir.util.algebra

/**
  * This is a marker trait. User must be able to prove that the op is associative.
  * @tparam T
  */
trait Semigroup[T] extends Magma[T]

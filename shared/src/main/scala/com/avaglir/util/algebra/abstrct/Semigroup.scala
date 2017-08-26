package com.avaglir.util.algebra.abstrct

/**
  * This is a marker trait. User must be able to prove that the op is associative.
 *
  * @tparam T
  */
trait Semigroup[@specialized(Specializable.AllNumeric) T] extends Magma[T]

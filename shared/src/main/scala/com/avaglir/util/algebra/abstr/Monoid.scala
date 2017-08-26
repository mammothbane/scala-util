package com.avaglir.util.algebra.abstr

trait Monoid[@specialized(Specializable.AllNumeric) T] extends Semigroup[T] {
  def identity: T
}

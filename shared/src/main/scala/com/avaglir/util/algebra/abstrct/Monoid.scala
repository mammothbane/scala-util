package com.avaglir.util.algebra.abstrct

trait Monoid[@specialized(Specializable.AllNumeric) T] extends Semigroup[T] {
  def identity: T
}

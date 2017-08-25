package com.avaglir.util.algebra

trait Monoid[@specialized(Specializable.AllNumeric) T] extends Semigroup[T] {
  def identity: T
}

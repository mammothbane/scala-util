package com.avaglir.util.algebra.abstrct

trait Group[@specialized(Specializable.AllNumeric) T] extends Monoid[T] {
  def inverse(a: T): T
}

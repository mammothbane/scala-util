package com.avaglir.util.algebra.abstrct

trait PartialGroupoid[@specialized(Specializable.AllNumeric) T] {
  def partial_op(t: T, u: T): Option[T]
}

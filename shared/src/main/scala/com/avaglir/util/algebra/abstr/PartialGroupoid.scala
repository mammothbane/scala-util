package com.avaglir.util.algebra.abstr

trait PartialGroupoid[@specialized(Specializable.AllNumeric) T] {
  def partial_op(t: T, u: T): Option[T]
}

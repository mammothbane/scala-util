package com.avaglir.util.algebra

trait Magma[@specialized(Specializable.AllNumeric) T] extends PartialGroupoid[T] {
  def op(t: T, u: T): T
  final override def partial_op(t: T, u: T): Option[T] = Some(op(t, u))
}

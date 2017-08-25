package com.avaglir.util.algebra

trait Magma[T] extends PartialGroupoid[T] {
  def op(t: T, u: T): T
  final override def partial_op(t: T, u: T): Option[T] = Some(op(t, u))
}

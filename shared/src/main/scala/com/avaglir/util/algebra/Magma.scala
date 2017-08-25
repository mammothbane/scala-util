package com.avaglir.util.algebra

trait Magma[@specialized(Specializable.AllNumeric) T] extends PartialGroupoid[T] {
  implicit class magmaExts[U: Magma](u: U) {
    def +(other: U): U = implicitly[Magma[U]].op(u, other)
  }

  def op(t: T, u: T): T
  final override def partial_op(t: T, u: T): Option[T] = Some(op(t, u))
}

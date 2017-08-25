package com.avaglir.util.algebra

trait PartialGroupoid[T] {
  def partial_op(t: T, u: T): Option[T]
}

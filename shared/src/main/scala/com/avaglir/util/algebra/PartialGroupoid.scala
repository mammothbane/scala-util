package com.avaglir.util.algebra

trait PartialGroupoid[T] {
  def op(t: T, u: T): Option[T]
}

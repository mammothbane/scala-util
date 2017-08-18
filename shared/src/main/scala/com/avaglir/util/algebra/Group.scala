package com.avaglir.util.algebra

trait Group[T] {
  this: Monoid[T] =>
  def inverse(a: T): T
}

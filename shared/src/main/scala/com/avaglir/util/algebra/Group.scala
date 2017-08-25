package com.avaglir.util.algebra

trait Group[T] extends Monoid[T] {
  def inverse(a: T): T
}

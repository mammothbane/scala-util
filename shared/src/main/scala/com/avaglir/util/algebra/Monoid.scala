package com.avaglir.util.algebra

trait Monoid[T] {
  this: Semigroup[T] =>
  def identity: T
}

package com.avaglir.util.algebra

trait Monoid[T] extends Semigroup[T] {
  def identity: T
}

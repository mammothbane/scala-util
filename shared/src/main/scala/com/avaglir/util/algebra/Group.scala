package com.avaglir.util.algebra

trait Group[T] extends Monoid[T] {
  def add(a: T, b: T): T = op(a, b)
  def inverse(a: T): T
}

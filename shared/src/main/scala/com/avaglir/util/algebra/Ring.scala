package com.avaglir.util.algebra

trait Ring[T] extends Group[T] with Commutative[T] {
  def one: T
  def zero: T = identity
  def mult(a: T, b: T): T
}

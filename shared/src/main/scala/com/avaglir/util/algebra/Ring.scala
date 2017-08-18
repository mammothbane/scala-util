package com.avaglir.util.algebra

trait Ring[T] extends AbelianGroup[T] {
  def one: T
  def zero: T = identity
  def mult(a: T, b: T): T
}

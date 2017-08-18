package com.avaglir.util.algebra

trait Magma[T] {
  def op(t: T, u: T): T
}

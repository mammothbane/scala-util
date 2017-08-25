package com.avaglir.util.algebra

trait Group[@specialized(Specializable.AllNumeric) T] extends Monoid[T] {
  implicit class groupExts[U: Group](u: U) {
    def unary_- : U = implicitly[Group[U]].inverse(u)
  }

  def inverse(a: T): T
}

package com.avaglir.util

package object algebra {
  type Groupoid[T] = Magma[T]
  type PartialMagma[T] = PartialGroupoid[T]
  type AbelianGroup[T] = Group[T] with Commutative[T]

  implicit def partialForMagma[T: Magma]: PartialGroupoid[T] = new PartialGroupoid[T] {
    override def op(t: T, u: T) = Some(implicitly[Magma[T]].op(t, u))
  }
}

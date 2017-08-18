package com.avaglir.util

package object algebra {
  type Groupoid[T] = Magma[T]
  type PartialMagma[T] = PartialGroupoid[T]

  implicit def partialForMagma[T: Magma]: PartialGroupoid[T] = new PartialGroupoid[T] {
    override def op(t: T) = Some(implicitly[Magma[T]].op(t))
  }
}

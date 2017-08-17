package com.avaglir.util.numeric

final case class UnitClamped[+T: Numeric](v: T) extends Clamped[T](v, implicitly[Numeric[T]].zero, implicitly[Numeric[T]].one)

package com.avaglir.util.numeric

class Clamped[T: Numeric](v: T,
                          min: T = implicitly[Numeric[T]].zero,
                          max: T = implicitly[Numeric[T]].one) {
  lazy val value: T = v.clamp(min, max)
  override def toString: String = s"$value"
}

final case class UnitClamped[T: Numeric](v: T) extends Clamped[T](v)

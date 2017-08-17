package com.avaglir.util.numeric

class Clamped[+T: Numeric](v: T,
                           val min: T,
                           val max: T) {
  lazy val value: T = v.clamp(min, max)
  override def toString: String = s"$value"
}

object Clamped {
  def apply[T: Numeric](v: T, min: Option[T] = None, max: Option[T] = None): Clamped[T] = {
    val num = implicitly[Numeric[T]]
    new Clamped(v, min.getOrElse(num.zero), max.getOrElse(num.one))
  }
}

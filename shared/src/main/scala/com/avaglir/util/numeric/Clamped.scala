package com.avaglir.util.numeric

import com.avaglir.util.numeric.Imports._

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

final case class UnitClamped[+T: Numeric](v: T) extends Clamped[T](v, implicitly[Numeric[T]].zero, implicitly[Numeric[T]].one)

package com.avaglir.util.numeric

object Imports {
  implicit class numericExt[T: Numeric](t: T) {
    private val num = implicitly[Numeric[T]]
    import num._

    def clamp: T = clamp(zero, one)
    def clamp(min: T, max: T): T = if (t < min) min else if (t > max) max else t
  }

  implicit class floatExt(f: Float) {
    def unitClamped = UnitClampedFloat(f)
  }

  implicit def clamped2Float(c: ClampedFloat): Float = c.value
  implicit def float2UnitClamped(f: Float): UnitClampedFloat = UnitClampedFloat(f)

  def maxOf[T: Ordering](args: T*): T = args.max
  def minOf[T: Ordering](args: T*): T = args.min
}

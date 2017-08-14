package com.avaglir.util.numeric

object Imports {
  implicit class numericExt[T: Numeric](t: T) {
    private val num = implicitly[Numeric[T]]
    import num._

    def clamp: T = clamp(zero, one)
    def clamp(min: T, max: T): T = if (t < min) min else if (t > max) max else t

    def unitClamped: UnitClamped[T] = UnitClamped(t)
  }

  implicit def clamped2T[T: Numeric](c: Clamped[T]): T = c.value
  implicit def T2UnitClamped[T: Numeric](f: T): UnitClamped[T] = UnitClamped[T](f)


  def maxOf[T: Ordering](args: T*): T = args.max
  def minOf[T: Ordering](args: T*): T = args.min

  implicit def clampedNumeric[T: Numeric]: Numeric[Clamped[T]] = new Numeric[Clamped[T]] {
    val num: Numeric[T] = implicitly[Numeric[T]]

    type U = Clamped[T]

    def plus(x: U, y: U): U = new U(num.plus(x.value, y.value), x.min, x.max)
    def minus(x: U, y: U): U = new U(num.minus(x.value, y.value), x.min, x.max)
    def times(x: U, y: U): U = new U(num.times(x.value, y.value), x.min, x.max)

    def compare(x: U, y: U): Int = num.compare(x.value, y.value)

    def negate(x: U): U = new U(num.negate(x.value), x.min, x.max)

    override def fromInt(x: Int) = new U(num.fromInt(x), num.zero, num.one)
    override def toInt(x: Clamped[T]): Int = num.toInt(x.value)
    override def toLong(x: Clamped[T]): Long = num.toLong(x.value)
    override def toFloat(x: Clamped[T]): Float = num.toFloat(x.value)
    override def toDouble(x: Clamped[T]): Double = num.toDouble(x.value)
  }
}

package com.avaglir.util

import scala.reflect.ClassTag

package object structure {
  implicit class vecIntegralExt[T: Integral: ClassTag, L <: VecLength : Length](v: Vector[T, L]) {
    val num: Integral[T] = implicitly[Integral[T]]
    import num._

    def half: Vector[T, L] = this / (one + one)
    def /[V](factor: V)(implicit conv: V => T): Vector[T, L] = v.map { _ / factor }
  }

  implicit class vecFractionalExt[T: Fractional: ClassTag, L <: VecLength : Length](v: Vector[T, L]) {
    val num: Fractional[T] = implicitly[Fractional[T]]
    import num._

    def half: Vector[T, L] = this / (one + one)
    def /[V](factor: V)(implicit conv: V => T): Vector[T, L] = v.map { _ / factor }
  }

  type Vector2[V] = Vector[V, VecLength.VL2.type]
  implicit class vec2Ext[T: Numeric: ClassTag](v: Vector2[T]) {
    val num: Numeric[T] = implicitly[Numeric[T]]
    import num._

    val x = v(0)
    val y = v(1)

    def adjacent: List[Vector2[T]] = adjacent(false)
    def adjacent(diag: Boolean): List[Vector2[T]] = {
      val cardinals = List[Vector2[T]](
        v + Vec2.UP,
        v + Vec2.DOWN,
        v + Vec2.LEFT,
        v + Vec2.RIGHT
      )
      if (!diag) return cardinals

      cardinals ++ List(
        v + Vec2.UP   + Vec2.RIGHT,
        v + Vec2.DOWN + Vec2.RIGHT,
        v + Vec2.UP   + Vec2.LEFT,
        v + Vec2.DOWN + Vec2.LEFT)
    }

    def octant: Int = {
      if (x > zero && y >= zero) { // first quadrant
        if (x >= y) 1
        else 2
      } else if (y > zero && x <= zero) {  // second quadrant
        if (-x >= y) 4
        else 3
      } else if (y <= zero && x < zero) { // third quadrant
        if (-x >= -y) 5
        else 6
      } else if (y < zero && x >= zero) { // fourth quadrant
        if (x >= -y) 8
        else 7
      } else 0 // origin
    }

    import com.avaglir.util.numeric._
    def clamp[V: Numeric: ClassTag, W: Numeric: ClassTag](min: Vector2[V], max: Vector2[W])(implicit convV: V => T, convW: W => T): Vector2[T] =
      new Vector2(minOf[T](max.x, maxOf[T](min.x, x)), minOf[T](max.y, maxOf[T](min.y, y)))
  }

  private type Vector3[V] = Vector[V, VecLength.VL3.type]
  implicit class vec3Ext[T: Numeric: ClassTag](v: Vector3[T]) {
    val num: Numeric[T] = implicitly[Numeric[T]]
    import num._

    val x = v(0)
    val y = v(1)
    val z = v(2)
    def cross[V: Numeric: ClassTag](other: Vector3[V])(implicit conv: (V) => T) =
      new Vector[T, VecLength.VL3.type](
        this.y * other.z - this.z * other.y,
        this.z * other.x - this.x * other.z,
        this.x * other.y - this.y * other.x)
  }

  type TokenizableTo[T] = ({type x[a] = Tokenizable[a, T]})#x
  trait Tokenizable[T, U] {
    def tokenize(t: T): Seq[U]
  }
}

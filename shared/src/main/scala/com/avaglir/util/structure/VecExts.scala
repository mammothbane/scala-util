package com.avaglir.util.structure

import scala.reflect.ClassTag

object VecExts {
  type Vector2[V] = Vector[V, VecLength.VL2.type]
  implicit class vec2Ext[T: Numeric: ClassTag](v: Vector2[T]) {
    val num: Numeric[T] = implicitly[Numeric[T]]
    import num._

    val x = v(0)
    val y = v(1)

    def adjacent: List[Vector2[T]] = adjacent(false)
    def adjacent(diag: Boolean): List[Vector2[T]] = {
      val cardinals = List[Vector2[T]](
        v + Vector2.UP,
        v + Vector2.DOWN,
        v + Vector2.LEFT,
        v + Vector2.RIGHT
      )
      if (!diag) return cardinals

      cardinals ++ List(
        v + Vector2.UP + Vector2.RIGHT,
        v + Vector2.DOWN + Vector2.RIGHT,
        v + Vector2.UP + Vector2.LEFT,
        v + Vector2.DOWN + Vector2.LEFT)
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

    import com.avaglir.util.numeric.Imports._
    def clamp[V: Numeric: ClassTag, W: Numeric: ClassTag](min: Vector2[V], max: Vector2[W])(implicit convV: V => T, convW: W => T): Vector2[T] =
      Vector2(minOf[T](max.x, maxOf[T](min.x, x)), minOf[T](max.y, maxOf[T](min.y, y)))
  }

  type Vector3[V] = Vector[V, VecLength.VL3.type]
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
}

object Vector2 {
  import VecExts.Vector2

  def apply[T: Numeric](t: (T, T)): Vector2[T] = Vector2(t._1, t._2)

  def UP[T: Numeric]: Vector2[T] = {
    val num = implicitly[Numeric[T]]
    import num._
    Vector2(zero, -one)
  }
  def DOWN[T: Numeric]: Vector2[T] = {
    val num = implicitly[Numeric[T]]
    import num._
    Vector2(zero, one)
  }
  def LEFT[T: Numeric]: Vector2[T] = {
    val num = implicitly[Numeric[T]]
    import num._
    Vector2(-one, zero)
  }
  def RIGHT[T: Numeric]: Vector2[T] = {
    val num = implicitly[Numeric[T]]
    import num._
    Vector2(one, zero)
  }
}
package com.avaglir.util.structure

import scala.reflect.ClassTag

object Vec2 {
  def apply[T: Numeric : ClassTag](t: (T, T)): Vector2[T] = new Vector2(t._1, t._2)

  def UP[T: Numeric : ClassTag]: Vector2[T] = {
    val num = implicitly[Numeric[T]]
    import num._
    new Vector2(zero, -one)
  }
  def DOWN[T: Numeric : ClassTag]: Vector2[T] = {
    val num = implicitly[Numeric[T]]
    import num._
    new Vector2(zero, one)
  }
  def LEFT[T: Numeric : ClassTag]: Vector2[T] = {
    val num = implicitly[Numeric[T]]
    import num._
    new Vector2(-one, zero)
  }
  def RIGHT[T: Numeric : ClassTag]: Vector2[T] = {
    val num = implicitly[Numeric[T]]
    import num._
    new Vector2(one, zero)
  }
}

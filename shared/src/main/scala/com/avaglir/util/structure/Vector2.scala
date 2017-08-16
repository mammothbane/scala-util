package com.avaglir.util.structure

object Vec2 {
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
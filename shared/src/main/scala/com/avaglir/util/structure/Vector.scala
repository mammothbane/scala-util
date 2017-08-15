package com.avaglir.util.structure

import com.avaglir.util.structure.VecExts._

import scala.reflect.ClassTag

class Vector[@specialized(Int, Double, Float) T: Numeric : ClassTag, L <: VecLength : VecLength.Length](private[util] val elems: T*) {
  assert(elems.length == dimension)
  private val num = implicitly[Numeric[T]]
  import num._

  def apply(i: Int): T = elems(i)

  def +[V](other: Vector[V, L])(implicit conv: V => T): Vector[T, L] = new Vector[T, L](elems.zip(other.elems).map{ case (a, b) => a + b }: _*)
  def -[V](other: Vector[V, L])(implicit conv: V => T): Vector[T, L] = new Vector[T, L](elems.zip(other.elems).map{ case (a, b) => a - b }: _*)
  def dot[V](other: Vector[V, L])(implicit conv: V => T): T = elems.zip(other.elems).map{ case (a, b) => a * b }.sum

  def unary_-(): Vector[T, L] = map(num.negate)

  def *[V](factor: V)(implicit conv: V => T): Vector[T, L] = map { elem => num.times(elem, factor) }

  def magnitude: Double = {
    val doubleRepr = this.map(_.toDouble)
    math.sqrt(doubleRepr.dot(doubleRepr))
  }
  def dimension: Int = implicitly[VecLength.Length[L]].length

  def normalize: Vector[Double, L] = map { _.toDouble } / magnitude
  def transpose = new Vector[T, L](elems.reverse: _*)

  def map[V: Numeric: ClassTag](fn: (T) => V): Vector[V, L] = new Vector[V, L](elems.map(fn): _*)
  def convert[V: Numeric: ClassTag](implicit conv: T => V): Vector[V, L] = map(conv)

  def componentsClamped[V](max: Vector[V, L])(implicit conv: V => T, convU: Int => T): Boolean = componentsClamped(max, Vector.ZERO[T, L])
  def componentsClamped[V, U](min: Vector[U, L], max: Vector[V, L])(implicit conv: V => T, convU: U => T): Boolean = this >= min && this < max

  override def toString: String = s"vec(${elems.map{_.toString}.mkString(", ")})"

  def <=[V](other: Vector[V, L])(implicit conv: V => T): Boolean = elems.zip(other.elems).forall { case (t, v) => t <= v }
  def >=[V](other: Vector[V, L])(implicit conv: V => T): Boolean = elems.zip(other.elems).forall { case (t, v) => t >= v }
  def <[V](other: Vector[V, L])(implicit conv: V => T): Boolean = elems.zip(other.elems).forall { case (t, v) => t < v }
  def >[V](other: Vector[V, L])(implicit conv: V => T): Boolean = elems.zip(other.elems).forall { case (t, v) => t > v }
}

object Vector {
  private def vecLength[L : VecLength.Length]: Int = implicitly[VecLength.Length[L]].length

  def ZERO[T: Numeric: ClassTag, L <: VecLength : VecLength.Length]: Vector[T, L] =
    new Vector[T, L](Array.fill[T](vecLength[L])(implicitly[Numeric[T]].zero): _*)

  def UNIT[T: Numeric: ClassTag, L <: VecLength : VecLength.Length]: Vector[T, L] =
    new Vector[T, L](Array.fill[T](vecLength[L])(implicitly[Numeric[T]].one): _*)
}

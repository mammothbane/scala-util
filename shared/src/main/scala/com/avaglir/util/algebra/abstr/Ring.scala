package com.avaglir.util.algebra.abstr

trait Ring[@specialized(Specializable.AllNumeric) T] extends Group[T] with Commutative[T] {
  def additiveGroup: Group[T] with Commutative[T]
  def multiplicativeSemigroup: Semigroup[T]

  final override def inverse(a: T): T = additiveGroup.inverse(a)
  final override val identity: T = additiveGroup.identity
  final override def op(t: T, u: T): T = additiveGroup.op(t, u)
  final def add(a: T, b: T): T = additiveGroup.op(a, b)

  final val zero: T = identity
  final def mult(a: T, b: T): T = multiplicativeSemigroup.op(a, b)
}

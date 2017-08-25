package com.avaglir.util.algebra

trait Ring[T] extends Group[T] with Commutative[T] {
  def additiveGroup: Group[T] with Commutative[T]
  def multiplicativeSemigroup: Semigroup[T]

  final def zero: T = identity
  final def mult(a: T, b: T): T = multiplicativeSemigroup.op(a, b)

  final override def inverse(a: T): T = additiveGroup.inverse(a)
  final override def identity: T = additiveGroup.identity
  final override def op(t: T, u: T): T = additiveGroup.op(t, u)
  final def add(a: T, b: T): T = additiveGroup.op(a, b)
}

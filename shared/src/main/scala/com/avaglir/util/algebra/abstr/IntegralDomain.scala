package com.avaglir.util.algebra.abstr

/**
  * User *must* be able to prove that multiplication on the ring is a commutative monoid with no zero-divisors.
  * @tparam T
  */
trait IntegralDomain[@specialized(Specializable.AllNumeric) T] extends Ring[T] {
  final val one: T = multiplicativeMonoid.identity
  def multiplicativeMonoid: Monoid[T] with Commutative[T]
  final override val multiplicativeSemigroup: Semigroup[T] = multiplicativeMonoid
}
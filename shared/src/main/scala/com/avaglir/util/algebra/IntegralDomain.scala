package com.avaglir.util.algebra

/**
  * User *must* be able to prove that multiplication on the ring is a commutative monoid with no zero-divisors.
  * @tparam T
  */
trait IntegralDomain[T] extends Ring[T] {
  final def one: T = multiplicativeMonoid.identity
  def multiplicativeMonoid: Monoid[T] with Commutative[T]
  final override def multiplicativeSemigroup: Semigroup[T] = multiplicativeMonoid
}
package com.avaglir.util.algebra

trait Field[T] extends IntegralDomain[T] {
  def multiplicativeGroup: Group[T] with Commutative[T]
  final override def multiplicativeMonoid: Monoid[T] with Commutative[T] = multiplicativeGroup
  final def div(a: T, b: T): T = if (b == zero) throw new ArithmeticException(s"Tried to divide $b by $zero") else mult(a, multiplicativeGroup.inverse(b))
}

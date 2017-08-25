package com.avaglir.util.algebra

trait Field[@specialized(Specializable.AllNumeric) T] extends IntegralDomain[T] {
  def multiplicativeGroup: Group[T] with Commutative[T]
  final override def multiplicativeMonoid: Monoid[T] with Commutative[T] = multiplicativeGroup

  final def uncheckedDiv(a: T, b: T): T = mult(a, multiplicativeGroup.inverse(b))
  final def div(a: T, b: T): T = if (b == zero) throw new ArithmeticException(s"Tried to divide $b by $zero") else mult(a, multiplicativeGroup.inverse(b))
}

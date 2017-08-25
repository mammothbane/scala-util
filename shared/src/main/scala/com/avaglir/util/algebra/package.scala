package com.avaglir.util

package object algebra {
  type Groupoid[T] = Magma[T]
  type PartialMagma[T] = PartialGroupoid[T]
  type AbelianGroup[T] = Group[T] with Commutative[T]

  implicit def numeric2IntegralDomain[T: Numeric]: IntegralDomain[T] = new IntegralDomain[T] {
    private val num = implicitly[Numeric[T]]

    override def multiplicativeMonoid: Monoid[T] with Commutative[T] = new Monoid[T] with Commutative[T] {
      override def identity: T = num.one
      override def op(t: T, u: T): T = num.times(t, u)
    }

    override def additiveGroup = new Group[T] with Commutative[T] {
      override def inverse(a: T): T = num.negate(a)
      override def identity: T = num.zero
      override def op(t: T, u: T): T = num.plus(t, u)
    }
  }

  implicit def fractional2Field[T: Fractional]: Field[T] = new Field[T] {
    private val intDom = numeric2IntegralDomain[T]
    private val frac = implicitly[Fractional[T]]

    override def multiplicativeGroup = new Group[T] with Commutative[T] {
      override def inverse(a: T): T = if (a == frac.zero) frac.zero else frac.div(frac.one, a)
      override def identity: T = frac.one
      override def op(t: T, u: T): T = frac.times(t, u)
    }

    override def additiveGroup: Group[T] with Commutative[T] = intDom.additiveGroup
  }
}

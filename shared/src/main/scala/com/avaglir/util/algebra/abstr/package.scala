package com.avaglir.util.algebra

import com.avaglir.util.{LengthLookup, TypeLength}

package object abstr {
  type Groupoid[T] = Magma[T]
  type PartialMagma[T] = PartialGroupoid[T]
  type AbelianGroup[T] = Group[T] with Commutative[T]

  implicit def numeric2IntegralDomain[T: Numeric]: IntegralDomain[T] = new IntegralDomain[T] {
    private val num = implicitly[Numeric[T]]

    override val multiplicativeMonoid: Monoid[T] with Commutative[T] = new Monoid[T] with Commutative[T] {
      override val identity: T = num.one
      override def op(t: T, u: T): T = num.times(t, u)
    }

    override val additiveGroup = new Group[T] with Commutative[T] {
      override def inverse(a: T): T = num.negate(a)
      override val identity: T = num.zero
      override def op(t: T, u: T): T = num.plus(t, u)
    }
  }

  implicit def fractional2Field[T: Fractional]: Field[T] = new Field[T] {
    private val intDom = numeric2IntegralDomain[T]
    private val frac = implicitly[Fractional[T]]

    override val multiplicativeGroup = new Group[T] with Commutative[T] {
      override def inverse(a: T): T = if (a == frac.zero) frac.zero else frac.div(frac.one, a)
      override def identity: T = frac.one
      override def op(t: T, u: T): T = frac.times(t, u)
    }

    override val additiveGroup: Group[T] with Commutative[T] = intDom.additiveGroup
  }

  implicit class fieldExts[U: Field](u: U) {
    def /(other: U): U = implicitly[Field[U]].div(u, other)
  }

  implicit class groupExts[U: Group](u: U) {
    val unary_- : U = implicitly[Group[U]].inverse(u)
    def -(other: U): U = u + -other
  }

  implicit class magmaExts[U: Magma](u: U) {
    def +(other: U): U = implicitly[Magma[U]].op(u, other)
  }

  implicit class ringExts[U: Ring](u: U) {
    def *(other: U): U = implicitly[Ring[U]].mult(u, other)
  }

  implicit def intModNGroup[N <: TypeLength : LengthLookup]: Group[IntMod[N]] = new Group[IntMod[N]] with Commutative[IntMod[N]] {
    private val modulus = implicitly[LengthLookup[N]].length
    override def inverse(a: IntMod[N]): IntMod[N] = IntMod[N](modulus - a.num)
    override val identity: IntMod[N] = IntMod[N](0)
    override def op(t: IntMod[N], u: IntMod[N]): IntMod[N] = IntMod[N]((t.num + u.num) % modulus)
  }
}

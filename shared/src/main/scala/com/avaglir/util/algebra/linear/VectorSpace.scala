package com.avaglir.util.algebra.linear

import com.avaglir.util.algebra.abstr.{Commutative, Field, Group}

/**
  * Make sure all vector space properties are maintained. In particular:
  *  - addition must form an abelian group
  *  - scalar multiplication must be distributive over vector addition
  *
  * @tparam V The vector type.
  * @tparam F Scalar type.
  */
trait VectorSpace[V, @specialized(Specializable.AllNumeric) F] {
  def additiveGroup(implicit field: Field[F]): Group[V] with Commutative[V]
  def scalarMult(f: F, v: V)(implicit field: Field[F]): V
}

object VectorSpace {
  trait Normed[V, @specialized(Specializable.AllNumeric) F] {
    this: VectorSpace[V, F] with SemiNormed[V, F] =>
    final def norm(v: V): F = seminorm(v)
  }

  trait SemiNormed[V, @specialized(Specializable.AllNumeric) F] {
    this: VectorSpace[V, F] =>
    def seminorm(v: V): F
  }

  trait InnerProduct[V, @specialized(Specializable.AllNumeric) F] {
    this: VectorSpace[V, F] =>
    def innerProduct(v: V, u: V)(implicit field: Field[F]): F
  }
}
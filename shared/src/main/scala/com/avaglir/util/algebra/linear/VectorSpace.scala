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
trait VectorSpace[V, F] {
  def field: Field[F]
  def additiveGroup: Group[V] with Commutative[V]

  def scalarMult(f: F, v: V): V

  trait Normed {
    this: VectorSpace[V, F] with SemiNormed =>
    final def norm(v: V): F = seminorm(v)
  }

  trait SemiNormed {
    this: VectorSpace[V, F] =>
    def seminorm(v: V): F
  }

  trait InnerProduct {
    this: VectorSpace[V, F] =>
    def innerProduct(v: V, u: V): F
  }
}

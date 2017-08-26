package com.avaglir.util.algebra.linear

import com.avaglir.util._
import com.avaglir.util.algebra.abstr._

import scalaz._

class EuclideanVectorSpace[F, D <: TypeLength : LengthLookup]
  extends VectorSpace[EuclideanVector[F, D], F]
    with VectorSpace.InnerProduct[EuclideanVector[F, D], F] {

  protected type Gen[A] = EuclideanVector[A, D]
  protected val func: Functor[Gen] = Functor[Gen]
  import func.functorSyntax._

  def dimension: Int = implicitly[LengthLookup[D]].length

  final override def additiveGroup(implicit field: Field[F]): Group[EuclideanVector[F, D]] with Commutative[EuclideanVector[F, D]] = new Group[EuclideanVector[F, D]] with Commutative[EuclideanVector[F, D]] {
    override def inverse(a: EuclideanVector[F, D]): EuclideanVector[F, D] = a.map { -_ }
    override def identity = EuclideanVector(Stream.fill(dimension)(field.zero): _*)
    override def op(t: EuclideanVector[F, D], u: EuclideanVector[F, D]) = EuclideanVector(t.elems.zip(u.elems).map { case (a, b) => a + b }: _*)
  }

  final override def scalarMult(f: F, v: EuclideanVector[F, D])(implicit field: Field[F]): EuclideanVector[F, D] = v.map { _ * f }

  final override def innerProduct(v: EuclideanVector[F, D], u: EuclideanVector[F, D])(implicit field: Field[F]): F =
    v.elems
      .zip(u.elems)
      .map { case (a, b) => field.mult(a, b) }
      .fold(field.zero) { case (acc, x) => field.add(acc, x) }
}

package com.avaglir.util.algebra.linear

import com.avaglir.util._
import com.avaglir.util.algebra.abstr._

class EuclideanSpace[F: Field, D <: TypeLength : LengthLookup] extends VectorSpace[EuclideanVector[F, D], F] {
  def dimension: Int = implicitly[LengthLookup[D]].length

  final override def field: Field[F] = implicitly[Field[F]]

  final override def additiveGroup: Group[EuclideanVector[F, D]] with Commutative[EuclideanVector[F, D]] = new Group[EuclideanVector[F, D]] with Commutative[EuclideanVector[F, D]] {
    override def inverse(a: EuclideanVector[F, D]): EuclideanVector[F, D] = a.map(field.additiveGroup.inverse)
    override def identity = EuclideanVector(Stream.fill(dimension)(field.zero): _*)
    override def op(t: EuclideanVector[F, D], u: EuclideanVector[F, D]) = EuclideanVector(t.elems.zip(u.elems).map { case (a, b) => a + b }: _*)
  }

  final override def scalarMult(f: F, v: EuclideanVector[F, D]): EuclideanVector[F, D] = v.map { _ * f }
}

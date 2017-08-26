package com.avaglir.util.algebra.linear

import com.avaglir.util._
import com.avaglir.util.algebra.abstr.Field
import com.avaglir.util.numeric.Sqrtable

abstract class EuclideanNormVectorSpace[F, D <: TypeLength : LengthLookup]
  extends EuclideanVectorSpace[F, D]
  with VectorSpace.Normed[EuclideanVector[F, D], F]
  with VectorSpace.SemiNormed[EuclideanVector[F, D], F] {

  def sqrt: Sqrtable[F]
  def field: Field[F]

  final override def seminorm(v: EuclideanVector[F, D]): F = {
    val sum = v.elems.fold(field.zero) { case (acc, x) => field.add(acc, x) }
    sqrt.sqrt(sum)
  }
}

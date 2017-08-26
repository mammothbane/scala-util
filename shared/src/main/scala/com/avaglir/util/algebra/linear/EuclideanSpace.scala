package com.avaglir.util.algebra.linear

import com.avaglir.util._
import com.avaglir.util.algebra.abstrct.Field

trait EuclideanSpace[F] extends VectorSpace[EuclideanVector[F, _], F] {
  override def field[F: Field] = ???
  override def additiveGroup = ???
  override def scalarMult(f: F, v: EuclideanVector[F, _]) = ???
}

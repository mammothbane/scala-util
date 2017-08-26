package com.avaglir.util.algebra.linear

import com.avaglir.util._
import com.avaglir.util.algebra.abstr.Group

case class EuclideanVector[T: Group, L <: TypeLength : LengthLookup] (private [util] val elems: T*) {
  assert(elems.length == implicitly[LengthLookup[L]].length)
  def apply(i: Int): T = elems(i)
  def map[U: Group](f: T => U): EuclideanVector[U, L] = EuclideanVector(elems.map(f): _*)
}

object EuclideanVector {

}

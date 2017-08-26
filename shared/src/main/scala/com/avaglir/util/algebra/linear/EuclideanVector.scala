package com.avaglir.util.algebra.linear

import com.avaglir.util._

case class EuclideanVector[@specialized(Specializable.AllNumeric) T, L <: TypeLength : LengthLookup] (private [util] val elems: T*) {
  assert(elems.length == implicitly[LengthLookup[L]].length)
  def apply(i: Int): T = elems(i)
}

object EuclideanVector {
}

package com.avaglir.util.algebra

import com.avaglir.util.{LengthLookup, TypeLength}

import scalaz.Functor

package object linear {
  implicit def functorEuclidean[L <: TypeLength : LengthLookup]: Functor[({type x[a]=EuclideanVector[a, L]})#x] = new Functor[({type x[a]=EuclideanVector[a, L]})#x] {
    override def map[A, B](fa: EuclideanVector[A, L])(f: (A) => B): EuclideanVector[B, L] = EuclideanVector[B, L](fa.elems.map(f): _*)
  }
}

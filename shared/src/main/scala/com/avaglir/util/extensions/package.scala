package com.avaglir.util

import com.avaglir.util.structure._

import scala.collection.GenTraversable

package object extensions {
  implicit class ary2dExt[T](a: Array[Array[T]]) {
    def apply(x: Vector2[Int]): T = a(x.x)(x.y)
    def extents = new Vector2(a.length, a.head.length)
  }

  implicit class genTExt[W](a: GenTraversable[W]) {
    def cartesianProduct[Z](other: GenTraversable[Z]): GenTraversable[(W, Z)] =
      a.flatMap { elem => other.map { inner => (elem, inner) } }
  }

  implicit class strExt(s: String) {
    def titleCase: String = s match {
      case "" => s
      case x if x.length == 1 => x.toUpperCase
      case _ => s(0).toUpper + s.substring(1)
    }
  }
}

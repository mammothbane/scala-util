package com.avaglir.util

import com.avaglir.util.color.Color
import com.avaglir.util.structure.VecExts._
import com.avaglir.util.structure.Vector2

import scala.collection.GenTraversable

package object extensions {
  implicit class ary2dExt[T](a: Array[Array[T]]) {
    def apply(x: IntVec): T = a(x.x)(x.y)
    def extents = Vector2(a.length, a.head.length)
  }

  implicit class genTExt[W](a: GenTraversable[W]) {
    def cartesianProduct[Z](other: GenTraversable[Z]): GenTraversable[(W, Z)] =
      a.flatMap { elem => other.map { inner => (elem, inner) } }
  }

  private final val colorRegex = "%c{#[0-9a-f]{6}}%b{#[0-9a-f]{6}}".r
  implicit class strExt(s: String) {
    def colorize(fg: Color = Color.WHITE, bg: Color = Color.BLACK): String = s"%c{${fg.hex}}%b{${bg.hex}}$s"
    def decolorize: String = colorRegex.replaceAllIn(s, "")
    def titleCase: String = s match {
      case "" => s
      case x if x.length == 1 => x.toUpperCase
      case _ => s(0).toUpper + s.substring(1)
    }
  }
}

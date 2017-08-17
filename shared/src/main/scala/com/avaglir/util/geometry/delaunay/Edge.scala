package com.avaglir.util.geometry.delaunay

import com.avaglir.util.structure.Vector2

case class Edge(a: Vector2[Double], b: Vector2[Double]) {
  def distance(v: Vector2[Double]) = {
    val ab = b - a
    var t = (v - a).dot(ab) / ab.dot(ab)
    if (t < 0.0d) t = 0.0d
    else if (t > 1.0d) t = 1.0d
    a + (ab * t)
  }

  def vertices = Set(a, b)
}

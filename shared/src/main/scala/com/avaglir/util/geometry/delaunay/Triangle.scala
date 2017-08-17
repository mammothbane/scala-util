package com.avaglir.util.geometry.delaunay

import com.avaglir.util.structure._

case class Triangle(a: Vector2[Double], b: Vector2[Double], c: Vector2[Double]) {
  private implicit class vec2Ext(v: Vector2[Double]) {
    def cross(other: Vector2[Double]) = v.y * other.x - v.x * other.y
  }

  lazy val edges = Array(
    Edge(a, b),
    Edge(b, c),
    Edge(c, a)
  )

  lazy val vertices = Array(a, b, c)

  def contains(point: Vector2[Double]): Boolean = {
    val pab = (point - a).cross(b - a)
    val pbc = (point - b).cross(c - b)
    if (!hasSameSign(pab, pbc)) return false
    val pca = (point - c).cross(a - c)
    if (!hasSameSign(pab, pca)) return false
    true
  }

  def isPointInCircumcircle(point: Vector2[Double]): Boolean = {
    val a11 = a.x - point.x
    val a21 = b.x - point.x
    val a31 = c.x - point.x
    val a12 = a.y - point.y
    val a22 = b.y - point.y
    val a32 = c.y - point.y
    val a13 = (a.x - point.x) * (a.x - point.x) + (a.y - point.y) * (a.y - point.y)
    val a23 = (b.x - point.x) * (b.x - point.x) + (b.y - point.y) * (b.y - point.y)
    val a33 = (c.x - point.x) * (c.x - point.x) + (c.y - point.y) * (c.y - point.y)
    val det = a11 * a22 * a33 + a12 * a23 * a31 + a13 * a21 * a32 - a13 * a22 * a31 - a12 * a21 * a33 - a11 * a23 * a32
    if (ccw) return det > 0.0d
    det < 0.0d
  }

  lazy val ccw: Boolean = {
    val a11 = a.x - c.x
    val a21 = b.x - c.x
    val a12 = a.y - c.y
    val a22 = b.y - c.y
    val det = a11 * a22 - a12 * a21
    det > 0
  }

  def -(edge: Edge): Array[Vector2[Double]] = vertices.filterNot(edge.vertices.contains)

  def neighbor(edge: Edge): Boolean = ((a eq edge.a) || (b eq edge.a) || (c eq edge.a)) && ((a eq edge.b) || (b eq edge.b) || (c eq edge.b))

  def nearestEdge(point: Vector2[Double]): Edge = edges.minBy { _.distance(point).magnitude }

  private def hasSameSign(a: Double, b: Double) = Math.signum(a) == Math.signum(b)

  override def toString: String = s"Triangle($a, $b, $c)"
}
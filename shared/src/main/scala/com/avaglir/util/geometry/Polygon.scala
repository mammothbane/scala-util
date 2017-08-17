package com.avaglir.util.geometry

import com.avaglir.util.structure._

import scala.annotation.tailrec
import scala.reflect.ClassTag

case class Polygon[T: Numeric: ClassTag] private (points: Vector2[T]*) {
  private val num = implicitly[Numeric[T]]
  import num._

  private lazy val horiz = points map { _.x }
  private lazy val vert = points map { _.y }

  private val div = num match {
    case n: Integral[T] => n.quot _
    case n: Fractional[T] => n.div _
  }

  @tailrec
  private def precompute(i: Int, j: Int, const: List[T], mult: List[T]): (Array[T], Array[T]) = i match {
    case _ if i == points.length => (const.toArray, mult.toArray)
    case _ if vert(i) == vert(j) => precompute(i + 1, i, horiz(i) :: const, num.zero :: mult)
    case _ =>
      val k = horiz(i) - div(vert(i) * horiz(j), vert(j) - vert(i)) + div(vert(i) * horiz(i),  vert(j) - vert(i))
      val m = div(horiz(j) - horiz(i), vert(j) - vert(i))
      precompute(i + 1, i, k :: const, m :: mult)
  }

  private lazy val (const, mul) = precompute(0, points.length - 1, List.empty, List.empty)

  def contains(point: Vector2[T]): Boolean = {
    @tailrec
    def inside(i: Int, j: Int, oddNodes: Boolean): Boolean = {
      i match {
        case _ if i == points.length => oddNodes
        case _ if vert(i) < point.y && vert(j) >= point.y || vert(j) < point.y && vert(i) >= point.y =>
          val odd = oddNodes ^ (point.y * mul(i) + const(i) < point.x)
          inside(i + 1, i, odd)
        case _ => inside(i + 1, i, oddNodes)
      }
    }

    inside(0, points.length - 1, oddNodes = false)
  }

  def svgPath: String = "M" + this.points.map { point => s"${point.x},${point.y}" }.mkString(" ") + "z"
}

object Polygon {
  def fromUnordered[T: Numeric : ClassTag](pts: Vector2[T]*)(implicit hull: Hull): Polygon[T] = Polygon(hull(pts: _*): _*)
}

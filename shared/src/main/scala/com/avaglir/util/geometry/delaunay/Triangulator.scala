package com.avaglir.util.geometry.delaunay

import com.avaglir.util.extensions._
import com.avaglir.util.structure._

import scala.collection.mutable

class Triangulator(val points: Set[Vector2[Double]]) {
  lazy val triangles: Set[Triangle] = {
    val coordMax = points.map { pt => Math.max(pt.x, pt.y) }.max * 16

    val p1 = new Vector2(0, 3 * coordMax)
    val p2 = new Vector2(3 * coordMax, 0)
    val p3 = new Vector2(-3 * coordMax, -3 * coordMax)
    val superTriangle = Triangle(p1, p2, p3)

    val triangles = mutable.Set(superTriangle)

    // TODO: make this tail-recursive
    def legalize(triangle: Triangle, edge: Edge, point: Vector2[Double]): Unit = {
      val neighbourTriangle = triangles.find { elt => elt != triangle && elt.neighbor(edge) }

      neighbourTriangle match {
        case Some(neighbor) =>
          if (!neighbor.isPointInCircumcircle(point)) return
          triangles -= triangle
          triangles -= neighbor

          val noneEdgeVertex = (neighbor - edge).head
          val firstTriangle = Triangle(noneEdgeVertex, edge.a, point)
          val secondTriangle = Triangle(noneEdgeVertex, edge.b, point)
          triangles += firstTriangle
          triangles += secondTriangle
          legalize(firstTriangle, Edge(noneEdgeVertex, edge.a), point)
          legalize(secondTriangle, Edge(noneEdgeVertex, edge.b), point)
        case None =>
      }
    }

    points.foreach { pt =>
      triangles.find { _.contains(pt) } match {
        case Some(tri) =>
          triangles -= tri
          tri.vertices.combinations(2).foreach { ary =>
            val newTri = Triangle(ary(0), ary(1), pt)
            triangles += newTri
            legalize(newTri, Edge(ary(0), ary(1)), pt)
          }

        case None =>
          val nearestEdge = triangles.map { _.nearestEdge(pt) }.minBy { _.distance(pt).magnitude }
          val neighbors = triangles.filter { _.neighbor(nearestEdge) }

          triangles --= neighbors

          neighbors
            .flatMap { _.vertices.filterNot(nearestEdge.vertices.contains) }
            .cartesianProduct(nearestEdge.vertices)
            .foreach {
              case (nonEdge, nearestEdgeVertex) =>
                val tri = Triangle(nearestEdgeVertex, nonEdge, pt)
                triangles += tri
                legalize(tri, Edge(nearestEdgeVertex, nonEdge), pt)
          }
      }
    }

    triangles.toSet
  }
}

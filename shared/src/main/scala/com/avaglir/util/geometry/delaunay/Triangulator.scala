package com.avaglir.util.geometry.delaunay

import java.util
import java.util.Collections

class DelaunayTriangulator(var pointSet: List[Nothing]) {
  this.triangleSoup = new Nothing
  private var triangleSoup = null

  def triangulate(): Unit = {
    triangleSoup = new Nothing
    if (pointSet == null || pointSet.size < 3) throw new Nothing("Less than three points in point set.")

    var maxOfAnyCoordinate = 0.0d
    import scala.collection.JavaConversions._
    for (vector <- getPointSet) {
      maxOfAnyCoordinate = Math.max(Math.max(vector.x, vector.y), maxOfAnyCoordinate)
    }
    maxOfAnyCoordinate *= 16.0d
    val p1 = new Nothing(0.0d, 3.0d * maxOfAnyCoordinate)
    val p2 = new Nothing(3.0d * maxOfAnyCoordinate, 0.0d)
    val p3 = new Nothing(-3.0d * maxOfAnyCoordinate, -3.0d * maxOfAnyCoordinate)
    val superTriangle = new Nothing(p1, p2, p3)
    triangleSoup.add(superTriangle)
    var i = 0
    while ( {
      i < pointSet.size
    }) {
      val triangle = triangleSoup.findContainingTriangle(pointSet.get(i))
      if (triangle == null) {
          val edge = triangleSoup.findNearestEdge(pointSet.get(i))
        val first = triangleSoup.findOneTriangleSharing(edge)
        val second = triangleSoup.findNeighbour(first, edge)
        val firstNoneEdgeVertex = first.getNoneEdgeVertex(edge)
        val secondNoneEdgeVertex = second.getNoneEdgeVertex(edge)
        triangleSoup.remove(first)
        triangleSoup.remove(second)
        val triangle1 = new Nothing(edge.a, firstNoneEdgeVertex, pointSet.get(i))
        val triangle2 = new Nothing(edge.b, firstNoneEdgeVertex, pointSet.get(i))
        val triangle3 = new Nothing(edge.a, secondNoneEdgeVertex, pointSet.get(i))
        val triangle4 = new Nothing(edge.b, secondNoneEdgeVertex, pointSet.get(i))
        triangleSoup.add(triangle1)
        triangleSoup.add(triangle2)
        triangleSoup.add(triangle3)
        triangleSoup.add(triangle4)
        legalizeEdge(triangle1, new Nothing(edge.a, firstNoneEdgeVertex), pointSet.get(i))
        legalizeEdge(triangle2, new Nothing(edge.b, firstNoneEdgeVertex), pointSet.get(i))
        legalizeEdge(triangle3, new Nothing(edge.a, secondNoneEdgeVertex), pointSet.get(i))
        legalizeEdge(triangle4, new Nothing(edge.b, secondNoneEdgeVertex), pointSet.get(i))
      }
      else {
        val a = triangle.a
        val b = triangle.b
        val c = triangle.c
        triangleSoup.remove(triangle)
        val first = new Nothing(a, b, pointSet.get(i))
        val second = new Nothing(b, c, pointSet.get(i))
        val third = new Nothing(c, a, pointSet.get(i))
        triangleSoup.add(first)
        triangleSoup.add(second)
        triangleSoup.add(third)
        legalizeEdge(first, new Nothing(a, b), pointSet.get(i))
        legalizeEdge(second, new Nothing(b, c), pointSet.get(i))
        legalizeEdge(third, new Nothing(c, a), pointSet.get(i))
      }
      {
        i += 1; i - 1
      }
    }
    triangleSoup.removeTrianglesUsing(superTriangle.a)
    triangleSoup.removeTrianglesUsing(superTriangle.b)
    triangleSoup.removeTrianglesUsing(superTriangle.c)
  }

  private def legalizeEdge(triangle: Nothing, edge: Nothing, newVertex: Nothing) = {
    val neighbourTriangle = triangleSoup.findNeighbour(triangle, edge)
    if (neighbourTriangle != null) if (neighbourTriangle.isPointInCircumcircle(newVertex)) {
      triangleSoup.remove(triangle)
      triangleSoup.remove(neighbourTriangle)
      val noneEdgeVertex = neighbourTriangle.getNoneEdgeVertex(edge)
      val firstTriangle = new Nothing(noneEdgeVertex, edge.a, newVertex)
      val secondTriangle = new Nothing(noneEdgeVertex, edge.b, newVertex)
      triangleSoup.add(firstTriangle)
      triangleSoup.add(secondTriangle)
      legalizeEdge(firstTriangle, new Nothing(noneEdgeVertex, edge.a), newVertex)
      legalizeEdge(secondTriangle, new Nothing(noneEdgeVertex, edge.b), newVertex)
    }
  }

  def shuffle(): Unit = Collections.shuffle(pointSet)

  def shuffle(permutation: Array[Int]): Unit = {
    val temp = new util.ArrayList[Nothing]
    var i = 0
    while ( {
      i < permutation.length
    }) {
      temp.add(pointSet.get(permutation(i)))
      {
        i += 1; i - 1
      }
    }
    pointSet = temp
  }

  def getPointSet: util.List[Nothing] = pointSet

  def getTriangles: util.List[Nothing] = triangleSoup.getTriangles
}

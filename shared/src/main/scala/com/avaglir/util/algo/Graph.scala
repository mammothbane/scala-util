package com.avaglir.util.algo

import scala.annotation.tailrec

object Graph {
  /**
    * Computes the distinct components of the graph represented by elems. Quadratic in time with no optimization.
    * @param elems The graph.
    * @param connected A function that determines whether two nodes in the graph are connected. Assumed to be transitive.
    * @tparam S The node type.
    * @return A list of connected components.
    */
  def components[S](elems: Set[S], connected: (S, S) => Boolean): List[Set[S]] = {
    @tailrec
    def buildComponent(remaining: Set[S], existing: List[Set[S]]): List[Set[S]] = remaining match {
      case x if x.isEmpty => existing
      case _ =>
        val component = remaining.tail.filter { connected(remaining.head, _) } + remaining.head
        buildComponent(remaining diff component, component :: existing)
    }
    buildComponent(elems, Nil)
  }
}

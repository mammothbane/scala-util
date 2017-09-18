package com.avaglir.util.structure

import scala.annotation.tailrec
import scala.collection.mutable



class RadixTree[K: Ordering, T: TokenizableTo[K], V](s: Map[T, V]) {
  case class RadixNode(var value: Option[V] = None, children: mutable.HashMap[K, RadixNode] = mutable.HashMap.empty) {
    @tailrec
    final def push(seq: Seq[K], value: V): Unit = seq match {
      case Seq.empty => this.value = Some(value)
      case head +: tail => this.children.get(head) match {
        case Some(x) => x.push(tail, value)
        case _ =>
      }
    }
  }

  private val root = RadixNode()

  s.map { case (k, v) => (implicitly[Tokenizable[T, K]].tokenize(k), v) }
    .foreach { case (k, v) => root.push(k, v) }

}

package com.avaglir.util.structure

import scala.annotation.tailrec
import scala.collection.mutable

case class RadixTree[K: Ordering, V] private (key: Option[K] = None,
                                              var value: Option[V] = None,
                                              children: mutable.HashMap[K, RadixTree[K, V]] = mutable.HashMap.empty) {
  @tailrec
  final def push(seq: Seq[K], value: V): Unit = seq match {
    case Seq.empty    => this.value = Some(value)
    case head +: tail => children.getOrElseUpdate(head, RadixTree(Some(head))).push(tail, value)
  }

  final def empty: Boolean = value.isEmpty && (children match {
    case x if x.isEmpty => true
    case x => x.values.forall(_.empty)
  })

  @tailrec
  final def remove(seq: Seq[K]): Option[V] = seq match {
    case Seq.empty =>
      val tmp = value

      value = None
      if (empty) children.clear()

      tmp

    case head +: tail =>
      children.get(head) match {
        case None => None
        case Some(x) => x.remove(tail)
      }
  }

  @tailrec
  final def apply(seq: Seq[K]): Option[V] = seq match {
    case Seq.empty    => value
    case head +: tail => children.get(head) match {
      case None => None
      case Some(x) => x.apply(tail)
    }
  }
}

object RadixTree {
  def apply[K: Ordering, T: TokenizableTo[K], V](s: Map[T, V]): RadixTree[K, V] = {
    val tokenizable = implicitly[Tokenizable[T, K]]

    val root = RadixTree[K, V]()
    s.foreach { case (k, v) => root.push(tokenizable(k), v) }

    root
  }
}
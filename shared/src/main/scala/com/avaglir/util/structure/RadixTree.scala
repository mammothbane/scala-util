package com.avaglir.util.structure

import scala.annotation.tailrec
import scala.collection.mutable

case class RadixTree[K: Ordering, V] private (key: Option[K] = None,
                                              var value: Option[V] = None,
                                              children: mutable.HashMap[K, RadixTree[K, V]] = mutable.HashMap.empty[K, RadixTree[K, V]]) {
  type Tokenizer[T] = Tokenizable[T, K]

  final def push[T: Tokenizer](t: T, value: V): Unit = pushRec(implicitly[Tokenizer[T]].apply(t), value)
  final def remove[T: Tokenizer](t: T): Option[V] = removeRec(implicitly[Tokenizer[T]].apply(t))
  final def apply[T: Tokenizer](t: T): Option[V] = applyRec(implicitly[Tokenizer[T]].apply(t))

  final def empty: Boolean = {
    @tailrec
    def recEmpty(checkList: List[RadixTree[K, V]]): Boolean = checkList match {
      case Nil => true
      case x :: xs => (x.value.isEmpty || x.children.isEmpty) && recEmpty(x.children.values.toList ::: xs)
    }

    recEmpty(this :: Nil)
  }

  @tailrec
  private def pushRec(seq: Seq[K], value: V): Unit = seq match {
    case x if x.isEmpty => this.value = Some(value)
    case head +: tail   => children
      .getOrElseUpdate(head, RadixTree[K, V](Some(head)))
      .pushRec(tail, value)
  }


  @tailrec
  private def removeRec(seq: Seq[K]): Option[V] = seq match {
    case x if x.isEmpty =>
      val tmp = value

      value = None
      if (empty) children.clear()

      tmp

    case head +: tail =>
      children.get(head) match {
        case None => None
        case Some(x) => x.removeRec(tail)
      }
  }

  @tailrec
  private def applyRec(seq: Seq[K]): Option[V] = seq match {
    case x if x.isEmpty => value
    case head +: tail   => children.get(head) match {
      case None => None
      case Some(x) => x.applyRec(tail)
    }
  }
}

object RadixTree {
  def apply[K: Ordering, T, V](s: Map[T, V])(implicit tokenizable: Tokenizable[T, K]): RadixTree[K, V] = {
    val root = RadixTree[K, V]()
    s.foreach { case (k, v) => root.push(tokenizable(k), v) }

    root
  }
}
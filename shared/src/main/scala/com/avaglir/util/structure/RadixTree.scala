package com.avaglir.util.structure

import scala.annotation.tailrec
import scala.collection.mutable

case class RadixTree[K, E, V] private
  (private[structure] var value: Option[V] = None,
   private[structure] val children: mutable.HashMap[E, RadixTree[K, E, V]] = mutable.HashMap.empty[E, RadixTree[K, E, V]])
  (implicit private val tokenConvert: TokenConvertible[K, E]) extends mutable.Map[K, V] {

  override def +=(kv: (K, V)): RadixTree.this.type = {
    pushRec(tokenConvert(kv._1), kv._2)
    this
  }

  override def -=(key: K): RadixTree.this.type = {
    removeRec(tokenConvert(key))
    this
  }

  override def get(key: K): Option[V] = {
    applyRec(tokenConvert(key))
  }

  override def iterator: Iterator[(K, V)] = new Iterator[(K, V)] {
    var pending: mutable.Queue[(Seq[E], RadixTree[K, E, V])] = mutable.Queue((Seq.empty, RadixTree.this))

    override def hasNext: Boolean = pending.nonEmpty
    override def next(): (K, V) = {
      while (pending.nonEmpty) { // this should be invariant given how this class is constructed
        val (seq, tree) = pending.dequeue()

        pending ++= tree.children.map { case (k, v) => (seq :+ k, v) }

        tree.value match {
          case None =>
          case Some(x) => return (tokenConvert(seq), x)
        }
      }
      throw new RuntimeException("Radix tree invariant breached")
    }
  }

  final override def isEmpty: Boolean = {
    @tailrec
    def recEmpty(checkList: List[RadixTree[K, E, V]]): Boolean = checkList match {
      case Nil => true
      case x :: xs => (x.value.isEmpty || x.children.isEmpty) && recEmpty(x.children.values.toList ::: xs)
    }

    recEmpty(this :: Nil)
  }

  @tailrec
  private def pushRec(seq: Seq[E], value: V): Unit = seq match {
    case x if x.isEmpty => this.value = Some(value)
    case head +: tail   => children
      .getOrElseUpdate(head, RadixTree[K, E, V]())
      .pushRec(tail, value)
  }


  @tailrec
  private def removeRec(seq: Seq[E]): Option[V] = seq match {
    case x if x.isEmpty =>
      val tmp = value

      value = None
      if (isEmpty) children.clear()

      tmp

    case head +: tail =>
      children.get(head) match {
        case None => None
        case Some(x) => x.removeRec(tail)
      }
  }

  @tailrec
  private def applyRec(seq: Seq[E]): Option[V] = seq match {
    case x if x.isEmpty => value
    case head +: tail   => children.get(head) match {
      case None => None
      case Some(x) => x.applyRec(tail)
    }
  }
}

object RadixTree {
  def apply[K, E, V](s: Map[K, V])(implicit tokenConvert: TokenConvertible[K, E]): RadixTree[K, E, V] = {
    val root = RadixTree[K, E, V]()
    s.foreach { case (k, v) => root(k) = v }

    root
  }
}
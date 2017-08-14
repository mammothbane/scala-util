package com.avaglir.util.structure

import scala.reflect.ClassTag

class RingBuffer[T: ClassTag](capacity: Int) {
  private val buf = new Array[T](capacity)
  private var head = 0
  private var tail = 0

  def isEmpty: Boolean = head == tail
  def nonEmpty: Boolean = head != tail
  def isFull: Boolean = (tail - head + capacity) % capacity == 1

  def push(t: T): Boolean = {
    if (isFull) return false
    buf(head) = t
    head += 1
    true
  }

  def pop: Option[T] = {
    if (isEmpty) return None

    val ret = buf(tail)
    tail = (tail + 1) % capacity
    Some(ret)
  }

  def size: Int = length
  def length: Int =
    if (head >= tail) (head - tail + capacity) % capacity
    else size - (tail - head)
}

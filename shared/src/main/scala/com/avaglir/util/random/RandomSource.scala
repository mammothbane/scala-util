package com.avaglir.util.random

trait RandomSource[T] {
  def random(implicit num: Numeric[T]): T
}

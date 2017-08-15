package com.avaglir.util

package object random {
  /**
    * Sample from a poisson distribution with expected value lambda.
    * Note: this implementation uses the Knuth algorithm, so it is slow for large lambda.
    * @param lambda The expected value.
    */
  def poisson(lambda: Int)
             (implicit randomSource: RandomSource[Double]): Int = {
    val l = math.exp(-lambda)
    var k = 0
    var p: Double = 1

    do {
      k += 1
      p *= randomSource.random
    } while (p > l)

    k - 1
  }
}

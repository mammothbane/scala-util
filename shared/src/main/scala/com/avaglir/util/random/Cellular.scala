package com.avaglir.util.random

import com.avaglir.util.numeric.Imports._
import com.avaglir.util.numeric.UnitClamped

object Cellular {
  sealed trait LifeState
  object LifeState {
    case object Alive extends LifeState
    case object Dead extends LifeState
  }

  private val birth = List(5, 6, 7, 8)
  private val survive = List(4, 5, 6, 7, 8)

  def generate(width: Int,
               height: Int,
               aliveProbability: UnitClamped[Float],
               random: RandomSource[Float],
               generations: Int = 4): Array[Array[_ <: LifeState]] = {

    var cur = Array.ofDim[Int](width, height)
    var next = Array.fill(width, height) { if (random.random > aliveProbability) 0 else 1 }

    def locValue(x: Int, y: Int): Int = {
      if (x < 0 || x >= width) return 0
      if (y < 0 || y >= height) return 0
      cur(x)(y)
    }

    def neighborCount(x: Int, y: Int): Int =
      locValue(x + 1, y) +
        locValue(x - 1, y) +
        locValue(x, y + 1) +
        locValue(x, y - 1) +
        locValue(x + 1, y + 1) +
        locValue(x - 1, y - 1) +
        locValue(x - 1, y + 1) +
        locValue(x + 1, y - 1)

    for (i <- 0 until generations) {
      val tmp = cur
      cur = next
      next = tmp

      for (x <- 0 until width; y <- 0 until height) {
        val neighbors = neighborCount(x, y)

        if (cur(x)(y) != 0 && survive.contains(neighbors) ||
          cur(x)(y) == 0 && birth.contains(neighbors)) { next(x)(y) = 1 }
        else next(x)(y) = 0
      }
    }

    next.map { _.map {
      case 1 => LifeState.Alive
      case 0 => LifeState.Dead
    }.toArray }
  }
}

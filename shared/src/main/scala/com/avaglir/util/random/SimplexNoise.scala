package com.avaglir.util.random

import com.avaglir.util.structure._

/**
  * Found here https://gist.githubusercontent.com/KdotJPG/b1270127455a94ac5d19/raw/df72ca5e708ebcb1d3c401a0617e1e288c76da82/OpenSimplexNoise.java
  */
object SimplexNoise {
  private val STRETCH_CONSTANT_2D: Double = -0.211324865405187 //(1/Math.sqrt(2+1)-1)/2;
  private val SQUISH_CONSTANT_2D: Double = 0.366025403784439 //(Math.sqrt(2+1)-1)/2;
  private val STRETCH_CONSTANT_3D: Double = -1.0 / 6 //(1/Math.sqrt(3+1)-1)/3;
  private val SQUISH_CONSTANT_3D: Double = 1.0 / 3 //(Math.sqrt(3+1)-1)/3;
  private val STRETCH_CONSTANT_4D: Double = -0.138196601125011 //(1/Math.sqrt(4+1)-1)/4;
  private val SQUISH_CONSTANT_4D: Double = 0.309016994374947 //(Math.sqrt(4+1)-1)/4;
  private val NORM_CONSTANT_2D: Double = 47
  private val NORM_CONSTANT_3D: Double = 103
  private val NORM_CONSTANT_4D: Double = 30
  private val DEFAULT_SEED: Long = 0

  private val GRAD_STR = "GRAD"
  private val PERM_STR = "PERM"

  def fastFloor(x: Double): Int = {
    val xi: Int = x.toInt
    if (x < xi) xi - 1 else xi
  }

  //Gradients for 2D. They approximate the directions to the
  //vertices of an octagon from the center.
  val gradients2D: Array[Byte] = Array[Byte](5, 2, 2, 5, -5, 2, -2, 5, 5, -2, 2, -5, -5, -2, -2, -5)
  //Gradients for 3D. They approximate the directions to the
  //vertices of a rhombicuboctahedron from the center, skewed so
  //that the triangular and square facets can be inscribed inside
  //circles of the same radius.
  val gradients3D: Array[Byte] = Array[Byte](-11, 4, 4, -4, 11, 4, -4, 4, 11, 11, 4, 4, 4, 11, 4, 4, 4, 11, -11, -4, 4, -4, -11, 4, -4, -4, 11, 11, -4, 4, 4, -11, 4, 4, -4, 11, -11, 4, -4, -4, 11, -4, -4, 4, -11, 11, 4, -4, 4, 11, -4, 4, 4, -11, -11, -4, -4, -4, -11, -4, -4, -4, -11, 11, -4, -4, 4, -11, -4, 4, -4, -11)
  //Gradients for 4D. They approximate the directions to the
  //vertices of a disprismatotesseractihexadecachoron from the center,
  //skewed so that the tetrahedral and cubic facets can be inscribed inside
  //spheres of the same radius.
  val gradients4D: Array[Byte] = Array[Byte](3, 1, 1, 1, 1, 3, 1, 1, 1, 1, 3, 1, 1, 1, 1, 3, -3, 1, 1, 1, -1, 3, 1, 1, -1, 1, 3, 1, -1, 1, 1, 3, 3, -1, 1, 1, 1, -3, 1, 1, 1, -1, 3, 1, 1, -1, 1, 3, -3, -1, 1, 1, -1, -3, 1, 1, -1, -1, 3, 1, -1, -1, 1, 3, 3, 1, -1, 1, 1, 3, -1, 1, 1, 1, -3, 1, 1, 1, -1, 3, -3, 1, -1, 1, -1, 3, -1, 1, -1, 1, -3, 1, -1, 1, -1, 3, 3, -1, -1, 1, 1, -3, -1, 1, 1, -1, -3, 1, 1, -1, -1, 3, -3, -1, -1, 1, -1, -3, -1, 1, -1, -1, -3, 1, -1, -1, -1, 3, 3, 1, 1, -1, 1, 3, 1, -1, 1, 1, 3, -1, 1, 1, 1, -3, -3, 1, 1, -1, -1, 3, 1, -1, -1, 1, 3, -1, -1, 1, 1, -3, 3, -1, 1, -1, 1, -3, 1, -1, 1, -1, 3, -1, 1, -1, 1, -3, -3, -1, 1, -1, -1, -3, 1, -1, -1, -1, 3, -1, -1, -1, 1, -3, 3, 1, -1, -1, 1, 3, -1, -1, 1, 1, -3, -1, 1, 1, -1, -3, -3, 1, -1, -1, -1, 3, -1, -1, -1, 1, -3, -1, -1, 1, -1, -3, 3, -1, -1, -1, 1, -3, -1, -1, 1, -1, -3, -1, 1, -1, -1, -3, -3, -1, -1, -1, -1, -3, -1, -1, -1, -1, -3, -1, -1, -1, -1, -3)

  def apply(s: Long = System.currentTimeMillis()): SimplexNoise = {
    var seed: Long = s

    val perm = new Array[Short](256)
    val permGradIndex3D = new Array[Short](256)
    val source: Array[Short] = (0 until 256).map{ _.toShort }.toArray

    seed = seed * 6364136223846793005L + 1442695040888963407L
    seed = seed * 6364136223846793005L + 1442695040888963407L
    seed = seed * 6364136223846793005L + 1442695040888963407L
    var i: Int = 255
    while (i >= 0) {
      seed = seed * 6364136223846793005L + 1442695040888963407L
      var r: Int = ((seed + 31) % (i + 1)).toInt
      if (r < 0) r += (i + 1)
      perm(i) = source(r)
      permGradIndex3D(i) = ((perm(i) % (SimplexNoise.gradients3D.length / 3)) * 3).toShort
      source(r) = source(i)
      i -= 1
    }

    SimplexNoise(perm, permGradIndex3D)
  }
}

case class SimplexNoise(perm: Array[Short], permGradIndex3D: Array[Short]) {
  def eval(v: Vector2[Int]): Double = eval(v.x, v.y)

  //2D OpenSimplex Noise.
  def eval(x: Double, y: Double): Double = {
    //Place input coordinates onto grid.
    val stretchOffset: Double = (x + y) * SimplexNoise.STRETCH_CONSTANT_2D
    val xs: Double = x + stretchOffset
    val ys: Double = y + stretchOffset
    //Floor to get grid coordinates of rhombus (stretched square) super-cell origin.
    var xsb: Int = SimplexNoise.fastFloor(xs)
    var ysb: Int = SimplexNoise.fastFloor(ys)
    //Skew out to get actual coordinates of rhombus origin. We'll need these later.
    val squishOffset: Double = (xsb + ysb) * SimplexNoise.SQUISH_CONSTANT_2D
    val xb: Double = xsb + squishOffset
    val yb: Double = ysb + squishOffset
    //Compute grid coordinates relative to rhombus origin.
    val xins: Double = xs - xsb
    val yins: Double = ys - ysb
    //Sum those together to get a value that determines which region we're in.
    val inSum: Double = xins + yins
    //Positions relative to origin point.
    var dx0: Double = x - xb
    var dy0: Double = y - yb
    //We'll be defining these inside the next block and using them afterwards.
    var dx_ext: Double = .0
    var dy_ext: Double = .0
    var xsv_ext: Int = 0
    var ysv_ext: Int = 0
    var value: Double = 0
    //Contribution (1,0)
    val dx1: Double = dx0 - 1 - SimplexNoise.SQUISH_CONSTANT_2D
    val dy1: Double = dy0 - 0 - SimplexNoise.SQUISH_CONSTANT_2D
    var attn1: Double = 2 - dx1 * dx1 - dy1 * dy1
    if (attn1 > 0) {
      attn1 *= attn1
      value += attn1 * attn1 * extrapolate(xsb + 1, ysb, dx1, dy1)
    }
    //Contribution (0,1)
    val dx2: Double = dx0 - 0 - SimplexNoise.SQUISH_CONSTANT_2D
    val dy2: Double = dy0 - 1 - SimplexNoise.SQUISH_CONSTANT_2D
    var attn2: Double = 2 - dx2 * dx2 - dy2 * dy2
    if (attn2 > 0) {
      attn2 *= attn2
      value += attn2 * attn2 * extrapolate(xsb, ysb + 1, dx2, dy2)
    }
    if (inSum <= 1) {
      //We're inside the triangle (2-Simplex) at (0,0)
      val zins: Double = 1 - inSum
      if (zins > xins || zins > yins) {
        //(0,0) is one of the closest two triangular vertices
        if (xins > yins) {
          xsv_ext = xsb + 1
          ysv_ext = ysb - 1
          dx_ext = dx0 - 1
          dy_ext = dy0 + 1
        }
        else {
          xsv_ext = xsb - 1
          ysv_ext = ysb + 1
          dx_ext = dx0 + 1
          dy_ext = dy0 - 1
        }
      }
      else {
        //(1,0) and (0,1) are the closest two vertices.
        xsv_ext = xsb + 1
        ysv_ext = ysb + 1
        dx_ext = dx0 - 1 - 2 * SimplexNoise.SQUISH_CONSTANT_2D
        dy_ext = dy0 - 1 - 2 * SimplexNoise.SQUISH_CONSTANT_2D
      }
    }
    else {
      //We're inside the triangle (2-Simplex) at (1,1)
      val zins: Double = 2 - inSum
      if (zins < xins || zins < yins) {
        //(0,0) is one of the closest two triangular vertices
        if (xins > yins) {
          xsv_ext = xsb + 2
          ysv_ext = ysb
          dx_ext = dx0 - 2 - 2 * SimplexNoise.SQUISH_CONSTANT_2D
          dy_ext = dy0 + 0 - 2 * SimplexNoise.SQUISH_CONSTANT_2D
        }
        else {
          xsv_ext = xsb
          ysv_ext = ysb + 2
          dx_ext = dx0 + 0 - 2 * SimplexNoise.SQUISH_CONSTANT_2D
          dy_ext = dy0 - 2 - 2 * SimplexNoise.SQUISH_CONSTANT_2D
        }
      }
      else {
        //(1,0) and (0,1) are the closest two vertices.
        dx_ext = dx0
        dy_ext = dy0
        xsv_ext = xsb
        ysv_ext = ysb
      }
      xsb += 1
      ysb += 1
      dx0 = dx0 - 1 - 2 * SimplexNoise.SQUISH_CONSTANT_2D
      dy0 = dy0 - 1 - 2 * SimplexNoise.SQUISH_CONSTANT_2D
    }
    //Contribution (0,0) or (1,1)
    var attn0: Double = 2 - dx0 * dx0 - dy0 * dy0
    if (attn0 > 0) {
      attn0 *= attn0
      value += attn0 * attn0 * extrapolate(xsb, ysb, dx0, dy0)
    }
    //Extra Vertex
    var attn_ext: Double = 2 - dx_ext * dx_ext - dy_ext * dy_ext
    if (attn_ext > 0) {
      attn_ext *= attn_ext
      value += attn_ext * attn_ext * extrapolate(xsv_ext, ysv_ext, dx_ext, dy_ext)
    }

    val out = value / SimplexNoise.NORM_CONSTANT_2D

    (out + 1) / 2 // range 0 to 1
  }

  //3D OpenSimplex Noise.
  def eval(x: Double, y: Double, z: Double): Double = {
    //Place input coordinates on simplectic honeycomb.
    val stretchOffset: Double = (x + y + z) * SimplexNoise.STRETCH_CONSTANT_3D
    val xs: Double = x + stretchOffset
    val ys: Double = y + stretchOffset
    val zs: Double = z + stretchOffset
    //Floor to get simplectic honeycomb coordinates of rhombohedron (stretched cube) super-cell origin.
    val xsb: Int = SimplexNoise.fastFloor(xs)
    val ysb: Int = SimplexNoise.fastFloor(ys)
    val zsb: Int = SimplexNoise.fastFloor(zs)
    //Skew out to get actual coordinates of rhombohedron origin. We'll need these later.
    val squishOffset: Double = (xsb + ysb + zsb) * SimplexNoise.SQUISH_CONSTANT_3D
    val xb: Double = xsb + squishOffset
    val yb: Double = ysb + squishOffset
    val zb: Double = zsb + squishOffset
    //Compute simplectic honeycomb coordinates relative to rhombohedral origin.
    val xins: Double = xs - xsb
    val yins: Double = ys - ysb
    val zins: Double = zs - zsb
    //Sum those together to get a value that determines which region we're in.
    val inSum: Double = xins + yins + zins
    //Positions relative to origin point.
    var dx0: Double = x - xb
    var dy0: Double = y - yb
    var dz0: Double = z - zb
    //We'll be defining these inside the next block and using them afterwards.
    var dx_ext0: Double = .0
    var dy_ext0: Double = .0
    var dz_ext0: Double = .0
    var dx_ext1: Double = .0
    var dy_ext1: Double = .0
    var dz_ext1: Double = .0
    var xsv_ext0: Int = 0
    var ysv_ext0: Int = 0
    var zsv_ext0: Int = 0
    var xsv_ext1: Int = 0
    var ysv_ext1: Int = 0
    var zsv_ext1: Int = 0
    var value: Double = 0
    if (inSum <= 1) {
      //We're inside the tetrahedron (3-Simplex) at (0,0,0)
      //Determine which two of (0,0,1), (0,1,0), (1,0,0) are closest.
      var aPoint: Byte = 0x01
      var aScore: Double = xins
      var bPoint: Byte = 0x02
      var bScore: Double = yins
      if (aScore >= bScore && zins > bScore) {
        bScore = zins
        bPoint = 0x04
      }
      else if (aScore < bScore && zins > aScore) {
        aScore = zins
        aPoint = 0x04
      }
      //Now we determine the two lattice points not part of the tetrahedron that may contribute.
      //This depends on the closest two tetrahedral vertices, including (0,0,0)
      val wins: Double = 1 - inSum
      if (wins > aScore || wins > bScore) {
        //(0,0,0) is one of the closest two tetrahedral vertices.
        val c: Byte = if (bScore > aScore) bPoint else aPoint //Our other closest vertex is the closest out of a and b.
        if ((c & 0x01) == 0) {
          xsv_ext0 = xsb - 1
          xsv_ext1 = xsb
          dx_ext0 = dx0 + 1
          dx_ext1 = dx0
        }
        else {
          xsv_ext0 = xsb + 1
          xsv_ext1 = xsb + 1
          dx_ext0 = dx0 - 1
          dx_ext1 = dx0 - 1
        }
        if ((c & 0x02) == 0) {
          ysv_ext0 = ysb
          ysv_ext1 = ysb
          dy_ext0 = dy0
          dy_ext1 = dy0
          if ((c & 0x01) == 0) {
            ysv_ext1 -= 1
            dy_ext1 += 1
          }
          else {
            ysv_ext0 -= 1
            dy_ext0 += 1
          }
        }
        else {
          ysv_ext0 = ysb + 1
          ysv_ext1 = ysb + 1
          dy_ext0 = dy0
          dy_ext1 = dy0 - 1
        }
        if ((c & 0x04) == 0) {
          zsv_ext0 = zsb
          zsv_ext1 = zsb - 1
          dz_ext0 = dz0
          dz_ext1 = dz0 + 1
        }
        else {
          zsv_ext0 = zsb + 1
          zsv_ext1 = zsb + 1
          dz_ext0 = dz0 - 1
          dz_ext1 = dz0 - 1
        }
      }
      else {
        //(0,0,0) is not one of the closest two tetrahedral vertices.
        val c: Byte = (aPoint | bPoint).toByte //Our two extra vertices are determined by the closest two.
        if ((c & 0x01) == 0) {
          xsv_ext0 = xsb
          xsv_ext1 = xsb - 1
          dx_ext0 = dx0 - 2 * SimplexNoise.SQUISH_CONSTANT_3D
          dx_ext1 = dx0 + 1 - SimplexNoise.SQUISH_CONSTANT_3D
        }
        else {
          xsv_ext0 = xsb + 1
          xsv_ext1 = xsb + 1
          dx_ext0 = dx0 - 1 - 2 * SimplexNoise.SQUISH_CONSTANT_3D
          dx_ext1 = dx0 - 1 - SimplexNoise.SQUISH_CONSTANT_3D
        }
        if ((c & 0x02) == 0) {
          ysv_ext0 = ysb
          ysv_ext1 = ysb - 1
          dy_ext0 = dy0 - 2 * SimplexNoise.SQUISH_CONSTANT_3D
          dy_ext1 = dy0 + 1 - SimplexNoise.SQUISH_CONSTANT_3D
        }
        else {
          ysv_ext0 = ysb + 1
          ysv_ext1 = ysb + 1
          dy_ext0 = dy0 - 1 - 2 * SimplexNoise.SQUISH_CONSTANT_3D
          dy_ext1 = dy0 - 1 - SimplexNoise.SQUISH_CONSTANT_3D
        }
        if ((c & 0x04) == 0) {
          zsv_ext0 = zsb
          zsv_ext1 = zsb - 1
          dz_ext0 = dz0 - 2 * SimplexNoise.SQUISH_CONSTANT_3D
          dz_ext1 = dz0 + 1 - SimplexNoise.SQUISH_CONSTANT_3D
        }
        else {
          zsv_ext0 = zsb + 1
          zsv_ext1 = zsb + 1
          dz_ext0 = dz0 - 1 - 2 * SimplexNoise.SQUISH_CONSTANT_3D
          dz_ext1 = dz0 - 1 - SimplexNoise.SQUISH_CONSTANT_3D
        }
      }
      //Contribution (0,0,0)
      var attn0: Double = 2 - dx0 * dx0 - dy0 * dy0 - dz0 * dz0
      if (attn0 > 0) {
        attn0 *= attn0
        value += attn0 * attn0 * extrapolate(xsb, ysb, zsb, dx0, dy0, dz0)
      }
      //Contribution (1,0,0)
      val dx1: Double = dx0 - 1 - SimplexNoise.SQUISH_CONSTANT_3D
      val dy1: Double = dy0 - 0 - SimplexNoise.SQUISH_CONSTANT_3D
      val dz1: Double = dz0 - 0 - SimplexNoise.SQUISH_CONSTANT_3D
      var attn1: Double = 2 - dx1 * dx1 - dy1 * dy1 - dz1 * dz1
      if (attn1 > 0) {
        attn1 *= attn1
        value += attn1 * attn1 * extrapolate(xsb + 1, ysb, zsb, dx1, dy1, dz1)
      }
      //Contribution (0,1,0)
      val dx2: Double = dx0 - 0 - SimplexNoise.SQUISH_CONSTANT_3D
      val dy2: Double = dy0 - 1 - SimplexNoise.SQUISH_CONSTANT_3D
      val dz2: Double = dz1
      var attn2: Double = 2 - dx2 * dx2 - dy2 * dy2 - dz2 * dz2
      if (attn2 > 0) {
        attn2 *= attn2
        value += attn2 * attn2 * extrapolate(xsb, ysb + 1, zsb, dx2, dy2, dz2)
      }
      //Contribution (0,0,1)
      val dx3: Double = dx2
      val dy3: Double = dy1
      val dz3: Double = dz0 - 1 - SimplexNoise.SQUISH_CONSTANT_3D
      var attn3: Double = 2 - dx3 * dx3 - dy3 * dy3 - dz3 * dz3
      if (attn3 > 0) {
        attn3 *= attn3
        value += attn3 * attn3 * extrapolate(xsb, ysb, zsb + 1, dx3, dy3, dz3)
      }
    }
    else if (inSum >= 2) {
      //We're inside the tetrahedron (3-Simplex) at (1,1,1)
      //Determine which two tetrahedral vertices are the closest, out of (1,1,0), (1,0,1), (0,1,1) but not (1,1,1).
      var aPoint: Byte = 0x06
      var aScore: Double = xins
      var bPoint: Byte = 0x05
      var bScore: Double = yins
      if (aScore <= bScore && zins < bScore) {
        bScore = zins
        bPoint = 0x03
      }
      else if (aScore > bScore && zins < aScore) {
        aScore = zins
        aPoint = 0x03
      }
      //Now we determine the two lattice points not part of the tetrahedron that may contribute.
      //This depends on the closest two tetrahedral vertices, including (1,1,1)
      val wins: Double = 3 - inSum
      if (wins < aScore || wins < bScore) {
        //(1,1,1) is one of the closest two tetrahedral vertices.
        val c: Byte = if (bScore < aScore) bPoint else aPoint //Our other closest vertex is the closest out of a and b.
        if ((c & 0x01) != 0) {
          xsv_ext0 = xsb + 2
          xsv_ext1 = xsb + 1
          dx_ext0 = dx0 - 2 - 3 * SimplexNoise.SQUISH_CONSTANT_3D
          dx_ext1 = dx0 - 1 - 3 * SimplexNoise.SQUISH_CONSTANT_3D
        }
        else {
          xsv_ext0 = xsb
          xsv_ext1 = xsb
          dx_ext0 = dx0 - 3 * SimplexNoise.SQUISH_CONSTANT_3D
          dx_ext1 = dx0 - 3 * SimplexNoise.SQUISH_CONSTANT_3D
        }
        if ((c & 0x02) != 0) {
          ysv_ext0 = ysb + 1
          ysv_ext1 = ysb + 1
          dy_ext0 = dy0 - 1 - 3 * SimplexNoise.SQUISH_CONSTANT_3D
          dy_ext1 = dy0 - 1 - 3 * SimplexNoise.SQUISH_CONSTANT_3D
          if ((c & 0x01) != 0) {
            ysv_ext1 += 1
            dy_ext1 -= 1
          }
          else {
            ysv_ext0 += 1
            dy_ext0 -= 1
          }
        }
        else {
          ysv_ext0 = ysb
          ysv_ext1 = ysb
          dy_ext0 = dy0 - 3 * SimplexNoise.SQUISH_CONSTANT_3D
          dy_ext1 = dy0 - 3 * SimplexNoise.SQUISH_CONSTANT_3D
        }
        if ((c & 0x04) != 0) {
          zsv_ext0 = zsb + 1
          zsv_ext1 = zsb + 2
          dz_ext0 = dz0 - 1 - 3 * SimplexNoise.SQUISH_CONSTANT_3D
          dz_ext1 = dz0 - 2 - 3 * SimplexNoise.SQUISH_CONSTANT_3D
        }
        else {
          zsv_ext0 = zsb
          zsv_ext1 = zsb
          dz_ext0 = dz0 - 3 * SimplexNoise.SQUISH_CONSTANT_3D
          dz_ext1 = dz0 - 3 * SimplexNoise.SQUISH_CONSTANT_3D
        }
      }
      else {
        //(1,1,1) is not one of the closest two tetrahedral vertices.
        val c: Byte = (aPoint & bPoint).toByte //Our two extra vertices are determined by the closest two.
        if ((c & 0x01) != 0) {
          xsv_ext0 = xsb + 1
          xsv_ext1 = xsb + 2
          dx_ext0 = dx0 - 1 - SimplexNoise.SQUISH_CONSTANT_3D
          dx_ext1 = dx0 - 2 - 2 * SimplexNoise.SQUISH_CONSTANT_3D
        }
        else {
          xsv_ext0 = xsb
          xsv_ext1 = xsb
          dx_ext0 = dx0 - SimplexNoise.SQUISH_CONSTANT_3D
          dx_ext1 = dx0 - 2 * SimplexNoise.SQUISH_CONSTANT_3D
        }
        if ((c & 0x02) != 0) {
          ysv_ext0 = ysb + 1
          ysv_ext1 = ysb + 2
          dy_ext0 = dy0 - 1 - SimplexNoise.SQUISH_CONSTANT_3D
          dy_ext1 = dy0 - 2 - 2 * SimplexNoise.SQUISH_CONSTANT_3D
        }
        else {
          ysv_ext0 = ysb
          ysv_ext1 = ysb
          dy_ext0 = dy0 - SimplexNoise.SQUISH_CONSTANT_3D
          dy_ext1 = dy0 - 2 * SimplexNoise.SQUISH_CONSTANT_3D
        }
        if ((c & 0x04) != 0) {
          zsv_ext0 = zsb + 1
          zsv_ext1 = zsb + 2
          dz_ext0 = dz0 - 1 - SimplexNoise.SQUISH_CONSTANT_3D
          dz_ext1 = dz0 - 2 - 2 * SimplexNoise.SQUISH_CONSTANT_3D
        }
        else {
          zsv_ext0 = zsb
          zsv_ext1 = zsb
          dz_ext0 = dz0 - SimplexNoise.SQUISH_CONSTANT_3D
          dz_ext1 = dz0 - 2 * SimplexNoise.SQUISH_CONSTANT_3D
        }
      }
      //Contribution (1,1,0)
      val dx3: Double = dx0 - 1 - 2 * SimplexNoise.SQUISH_CONSTANT_3D
      val dy3: Double = dy0 - 1 - 2 * SimplexNoise.SQUISH_CONSTANT_3D
      val dz3: Double = dz0 - 0 - 2 * SimplexNoise.SQUISH_CONSTANT_3D
      var attn3: Double = 2 - dx3 * dx3 - dy3 * dy3 - dz3 * dz3
      if (attn3 > 0) {
        attn3 *= attn3
        value += attn3 * attn3 * extrapolate(xsb + 1, ysb + 1, zsb + 0, dx3, dy3, dz3)
      }
      //Contribution (1,0,1)
      val dx2: Double = dx3
      val dy2: Double = dy0 - 0 - 2 * SimplexNoise.SQUISH_CONSTANT_3D
      val dz2: Double = dz0 - 1 - 2 * SimplexNoise.SQUISH_CONSTANT_3D
      var attn2: Double = 2 - dx2 * dx2 - dy2 * dy2 - dz2 * dz2
      if (attn2 > 0) {
        attn2 *= attn2
        value += attn2 * attn2 * extrapolate(xsb + 1, ysb + 0, zsb + 1, dx2, dy2, dz2)
      }
      //Contribution (0,1,1)
      val dx1: Double = dx0 - 0 - 2 * SimplexNoise.SQUISH_CONSTANT_3D
      val dy1: Double = dy3
      val dz1: Double = dz2
      var attn1: Double = 2 - dx1 * dx1 - dy1 * dy1 - dz1 * dz1
      if (attn1 > 0) {
        attn1 *= attn1
        value += attn1 * attn1 * extrapolate(xsb + 0, ysb + 1, zsb + 1, dx1, dy1, dz1)
      }
      //Contribution (1,1,1)
      dx0 = dx0 - 1 - 3 * SimplexNoise.SQUISH_CONSTANT_3D
      dy0 = dy0 - 1 - 3 * SimplexNoise.SQUISH_CONSTANT_3D
      dz0 = dz0 - 1 - 3 * SimplexNoise.SQUISH_CONSTANT_3D
      var attn0: Double = 2 - dx0 * dx0 - dy0 * dy0 - dz0 * dz0
      if (attn0 > 0) {
        attn0 *= attn0
        value += attn0 * attn0 * extrapolate(xsb + 1, ysb + 1, zsb + 1, dx0, dy0, dz0)
      }
    }
    else {
      //We're inside the octahedron (Rectified 3-Simplex) in between.
      var aScore: Double = .0
      var aPoint: Byte = 0
      var aIsFurtherSide: Boolean = false
      var bScore: Double = .0
      var bPoint: Byte = 0
      var bIsFurtherSide: Boolean = false
      //Decide between point (0,0,1) and (1,1,0) as closest
      val p1: Double = xins + yins
      if (p1 > 1) {
        aScore = p1 - 1
        aPoint = 0x03
        aIsFurtherSide = true
      }
      else {
        aScore = 1 - p1
        aPoint = 0x04
        aIsFurtherSide = false
      }
      //Decide between point (0,1,0) and (1,0,1) as closest
      val p2: Double = xins + zins
      if (p2 > 1) {
        bScore = p2 - 1
        bPoint = 0x05
        bIsFurtherSide = true
      }
      else {
        bScore = 1 - p2
        bPoint = 0x02
        bIsFurtherSide = false
      }
      //The closest out of the two (1,0,0) and (0,1,1) will replace the furthest out of the two decided above, if closer.
      val p3: Double = yins + zins
      if (p3 > 1) {
        val score: Double = p3 - 1
        if (aScore <= bScore && aScore < score) {
          aScore = score
          aPoint = 0x06
          aIsFurtherSide = true
        }
        else if (aScore > bScore && bScore < score) {
          bScore = score
          bPoint = 0x06
          bIsFurtherSide = true
        }
      }
      else {
        val score: Double = 1 - p3
        if (aScore <= bScore && aScore < score) {
          aScore = score
          aPoint = 0x01
          aIsFurtherSide = false
        }
        else if (aScore > bScore && bScore < score) {
          bScore = score
          bPoint = 0x01
          bIsFurtherSide = false
        }
      }
      //Where each of the two closest points are determines how the extra two vertices are calculated.
      if (aIsFurtherSide == bIsFurtherSide) {
        if (aIsFurtherSide) {
          //Both closest points on (1,1,1) side
          //One of the two extra points is (1,1,1)
          dx_ext0 = dx0 - 1 - 3 * SimplexNoise.SQUISH_CONSTANT_3D
          dy_ext0 = dy0 - 1 - 3 * SimplexNoise.SQUISH_CONSTANT_3D
          dz_ext0 = dz0 - 1 - 3 * SimplexNoise.SQUISH_CONSTANT_3D
          xsv_ext0 = xsb + 1
          ysv_ext0 = ysb + 1
          zsv_ext0 = zsb + 1
          //Other extra point is based on the shared axis.
          val c: Byte = (aPoint & bPoint).toByte
          if ((c & 0x01) != 0) {
            dx_ext1 = dx0 - 2 - 2 * SimplexNoise.SQUISH_CONSTANT_3D
            dy_ext1 = dy0 - 2 * SimplexNoise.SQUISH_CONSTANT_3D
            dz_ext1 = dz0 - 2 * SimplexNoise.SQUISH_CONSTANT_3D
            xsv_ext1 = xsb + 2
            ysv_ext1 = ysb
            zsv_ext1 = zsb
          }
          else if ((c & 0x02) != 0) {
            dx_ext1 = dx0 - 2 * SimplexNoise.SQUISH_CONSTANT_3D
            dy_ext1 = dy0 - 2 - 2 * SimplexNoise.SQUISH_CONSTANT_3D
            dz_ext1 = dz0 - 2 * SimplexNoise.SQUISH_CONSTANT_3D
            xsv_ext1 = xsb
            ysv_ext1 = ysb + 2
            zsv_ext1 = zsb
          }
          else {
            dx_ext1 = dx0 - 2 * SimplexNoise.SQUISH_CONSTANT_3D
            dy_ext1 = dy0 - 2 * SimplexNoise.SQUISH_CONSTANT_3D
            dz_ext1 = dz0 - 2 - 2 * SimplexNoise.SQUISH_CONSTANT_3D
            xsv_ext1 = xsb
            ysv_ext1 = ysb
            zsv_ext1 = zsb + 2
          }
        }
        else {
          //Both closest points on (0,0,0) side
          //One of the two extra points is (0,0,0)
          dx_ext0 = dx0
          dy_ext0 = dy0
          dz_ext0 = dz0
          xsv_ext0 = xsb
          ysv_ext0 = ysb
          zsv_ext0 = zsb
          //Other extra point is based on the omitted axis.
          val c: Byte = (aPoint | bPoint).toByte
          if ((c & 0x01) == 0) {
            dx_ext1 = dx0 + 1 - SimplexNoise.SQUISH_CONSTANT_3D
            dy_ext1 = dy0 - 1 - SimplexNoise.SQUISH_CONSTANT_3D
            dz_ext1 = dz0 - 1 - SimplexNoise.SQUISH_CONSTANT_3D
            xsv_ext1 = xsb - 1
            ysv_ext1 = ysb + 1
            zsv_ext1 = zsb + 1
          }
          else if ((c & 0x02) == 0) {
            dx_ext1 = dx0 - 1 - SimplexNoise.SQUISH_CONSTANT_3D
            dy_ext1 = dy0 + 1 - SimplexNoise.SQUISH_CONSTANT_3D
            dz_ext1 = dz0 - 1 - SimplexNoise.SQUISH_CONSTANT_3D
            xsv_ext1 = xsb + 1
            ysv_ext1 = ysb - 1
            zsv_ext1 = zsb + 1
          }
          else {
            dx_ext1 = dx0 - 1 - SimplexNoise.SQUISH_CONSTANT_3D
            dy_ext1 = dy0 - 1 - SimplexNoise.SQUISH_CONSTANT_3D
            dz_ext1 = dz0 + 1 - SimplexNoise.SQUISH_CONSTANT_3D
            xsv_ext1 = xsb + 1
            ysv_ext1 = ysb + 1
            zsv_ext1 = zsb - 1
          }
        }
      }
      else {
        //One point on (0,0,0) side, one point on (1,1,1) side
        var c1: Byte = 0
        var c2: Byte = 0
        if (aIsFurtherSide) {
          c1 = aPoint
          c2 = bPoint
        }
        else {
          c1 = bPoint
          c2 = aPoint
        }
        //One contribution is a permutation of (1,1,-1)
        if ((c1 & 0x01) == 0) {
          dx_ext0 = dx0 + 1 - SimplexNoise.SQUISH_CONSTANT_3D
          dy_ext0 = dy0 - 1 - SimplexNoise.SQUISH_CONSTANT_3D
          dz_ext0 = dz0 - 1 - SimplexNoise.SQUISH_CONSTANT_3D
          xsv_ext0 = xsb - 1
          ysv_ext0 = ysb + 1
          zsv_ext0 = zsb + 1
        }
        else if ((c1 & 0x02) == 0) {
          dx_ext0 = dx0 - 1 - SimplexNoise.SQUISH_CONSTANT_3D
          dy_ext0 = dy0 + 1 - SimplexNoise.SQUISH_CONSTANT_3D
          dz_ext0 = dz0 - 1 - SimplexNoise.SQUISH_CONSTANT_3D
          xsv_ext0 = xsb + 1
          ysv_ext0 = ysb - 1
          zsv_ext0 = zsb + 1
        }
        else {
          dx_ext0 = dx0 - 1 - SimplexNoise.SQUISH_CONSTANT_3D
          dy_ext0 = dy0 - 1 - SimplexNoise.SQUISH_CONSTANT_3D
          dz_ext0 = dz0 + 1 - SimplexNoise.SQUISH_CONSTANT_3D
          xsv_ext0 = xsb + 1
          ysv_ext0 = ysb + 1
          zsv_ext0 = zsb - 1
        }
        //One contribution is a permutation of (0,0,2)
        dx_ext1 = dx0 - 2 * SimplexNoise.SQUISH_CONSTANT_3D
        dy_ext1 = dy0 - 2 * SimplexNoise.SQUISH_CONSTANT_3D
        dz_ext1 = dz0 - 2 * SimplexNoise.SQUISH_CONSTANT_3D
        xsv_ext1 = xsb
        ysv_ext1 = ysb
        zsv_ext1 = zsb
        if ((c2 & 0x01) != 0) {
          dx_ext1 -= 2
          xsv_ext1 += 2
        }
        else if ((c2 & 0x02) != 0) {
          dy_ext1 -= 2
          ysv_ext1 += 2
        }
        else {
          dz_ext1 -= 2
          zsv_ext1 += 2
        }
      }
      //Contribution (1,0,0)
      val dx1: Double = dx0 - 1 - SimplexNoise.SQUISH_CONSTANT_3D
      val dy1: Double = dy0 - 0 - SimplexNoise.SQUISH_CONSTANT_3D
      val dz1: Double = dz0 - 0 - SimplexNoise.SQUISH_CONSTANT_3D
      var attn1: Double = 2 - dx1 * dx1 - dy1 * dy1 - dz1 * dz1
      if (attn1 > 0) {
        attn1 *= attn1
        value += attn1 * attn1 * extrapolate(xsb + 1, ysb + 0, zsb + 0, dx1, dy1, dz1)
      }
      //Contribution (0,1,0)
      val dx2: Double = dx0 - 0 - SimplexNoise.SQUISH_CONSTANT_3D
      val dy2: Double = dy0 - 1 - SimplexNoise.SQUISH_CONSTANT_3D
      val dz2: Double = dz1
      var attn2: Double = 2 - dx2 * dx2 - dy2 * dy2 - dz2 * dz2
      if (attn2 > 0) {
        attn2 *= attn2
        value += attn2 * attn2 * extrapolate(xsb + 0, ysb + 1, zsb + 0, dx2, dy2, dz2)
      }
      //Contribution (0,0,1)
      val dx3: Double = dx2
      val dy3: Double = dy1
      val dz3: Double = dz0 - 1 - SimplexNoise.SQUISH_CONSTANT_3D
      var attn3: Double = 2 - dx3 * dx3 - dy3 * dy3 - dz3 * dz3
      if (attn3 > 0) {
        attn3 *= attn3
        value += attn3 * attn3 * extrapolate(xsb + 0, ysb + 0, zsb + 1, dx3, dy3, dz3)
      }
      //Contribution (1,1,0)
      val dx4: Double = dx0 - 1 - 2 * SimplexNoise.SQUISH_CONSTANT_3D
      val dy4: Double = dy0 - 1 - 2 * SimplexNoise.SQUISH_CONSTANT_3D
      val dz4: Double = dz0 - 0 - 2 * SimplexNoise.SQUISH_CONSTANT_3D
      var attn4: Double = 2 - dx4 * dx4 - dy4 * dy4 - dz4 * dz4
      if (attn4 > 0) {
        attn4 *= attn4
        value += attn4 * attn4 * extrapolate(xsb + 1, ysb + 1, zsb + 0, dx4, dy4, dz4)
      }
      //Contribution (1,0,1)
      val dx5: Double = dx4
      val dy5: Double = dy0 - 0 - 2 * SimplexNoise.SQUISH_CONSTANT_3D
      val dz5: Double = dz0 - 1 - 2 * SimplexNoise.SQUISH_CONSTANT_3D
      var attn5: Double = 2 - dx5 * dx5 - dy5 * dy5 - dz5 * dz5
      if (attn5 > 0) {
        attn5 *= attn5
        value += attn5 * attn5 * extrapolate(xsb + 1, ysb + 0, zsb + 1, dx5, dy5, dz5)
      }
      //Contribution (0,1,1)
      val dx6: Double = dx0 - 0 - 2 * SimplexNoise.SQUISH_CONSTANT_3D
      val dy6: Double = dy4
      val dz6: Double = dz5
      var attn6: Double = 2 - dx6 * dx6 - dy6 * dy6 - dz6 * dz6
      if (attn6 > 0) {
        attn6 *= attn6
        value += attn6 * attn6 * extrapolate(xsb + 0, ysb + 1, zsb + 1, dx6, dy6, dz6)
      }
    }
    //First extra vertex
    var attn_ext0: Double = 2 - dx_ext0 * dx_ext0 - dy_ext0 * dy_ext0 - dz_ext0 * dz_ext0
    if (attn_ext0 > 0) {
      attn_ext0 *= attn_ext0
      value += attn_ext0 * attn_ext0 * extrapolate(xsv_ext0, ysv_ext0, zsv_ext0, dx_ext0, dy_ext0, dz_ext0)
    }
    //Second extra vertex
    var attn_ext1: Double = 2 - dx_ext1 * dx_ext1 - dy_ext1 * dy_ext1 - dz_ext1 * dz_ext1
    if (attn_ext1 > 0) {
      attn_ext1 *= attn_ext1
      value += attn_ext1 * attn_ext1 * extrapolate(xsv_ext1, ysv_ext1, zsv_ext1, dx_ext1, dy_ext1, dz_ext1)
    }
    return value / SimplexNoise.NORM_CONSTANT_3D
  }

  //4D OpenSimplex Noise.
  def eval(x: Double, y: Double, z: Double, w: Double): Double = {
    //Place input coordinates on simplectic honeycomb.
    val stretchOffset: Double = (x + y + z + w) * SimplexNoise.STRETCH_CONSTANT_4D
    val xs: Double = x + stretchOffset
    val ys: Double = y + stretchOffset
    val zs: Double = z + stretchOffset
    val ws: Double = w + stretchOffset
    //Floor to get simplectic honeycomb coordinates of rhombo-hypercube super-cell origin.
    val xsb: Int = SimplexNoise.fastFloor(xs)
    val ysb: Int = SimplexNoise.fastFloor(ys)
    val zsb: Int = SimplexNoise.fastFloor(zs)
    val wsb: Int = SimplexNoise.fastFloor(ws)
    //Skew out to get actual coordinates of stretched rhombo-hypercube origin. We'll need these later.
    val squishOffset: Double = (xsb + ysb + zsb + wsb) * SimplexNoise.SQUISH_CONSTANT_4D
    val xb: Double = xsb + squishOffset
    val yb: Double = ysb + squishOffset
    val zb: Double = zsb + squishOffset
    val wb: Double = wsb + squishOffset
    //Compute simplectic honeycomb coordinates relative to rhombo-hypercube origin.
    val xins: Double = xs - xsb
    val yins: Double = ys - ysb
    val zins: Double = zs - zsb
    val wins: Double = ws - wsb
    //Sum those together to get a value that determines which region we're in.
    val inSum: Double = xins + yins + zins + wins
    //Positions relative to origin point.
    var dx0: Double = x - xb
    var dy0: Double = y - yb
    var dz0: Double = z - zb
    var dw0: Double = w - wb
    //We'll be defining these inside the next block and using them afterwards.
    var dx_ext0: Double = .0
    var dy_ext0: Double = .0
    var dz_ext0: Double = .0
    var dw_ext0: Double = .0
    var dx_ext1: Double = .0
    var dy_ext1: Double = .0
    var dz_ext1: Double = .0
    var dw_ext1: Double = .0
    var dx_ext2: Double = .0
    var dy_ext2: Double = .0
    var dz_ext2: Double = .0
    var dw_ext2: Double = .0
    var xsv_ext0: Int = 0
    var ysv_ext0: Int = 0
    var zsv_ext0: Int = 0
    var wsv_ext0: Int = 0
    var xsv_ext1: Int = 0
    var ysv_ext1: Int = 0
    var zsv_ext1: Int = 0
    var wsv_ext1: Int = 0
    var xsv_ext2: Int = 0
    var ysv_ext2: Int = 0
    var zsv_ext2: Int = 0
    var wsv_ext2: Int = 0
    var value: Double = 0
    if (inSum <= 1) {
      //We're inside the pentachoron (4-Simplex) at (0,0,0,0)
      //Determine which two of (0,0,0,1), (0,0,1,0), (0,1,0,0), (1,0,0,0) are closest.
      var aPoint: Byte = 0x01
      var aScore: Double = xins
      var bPoint: Byte = 0x02
      var bScore: Double = yins
      if (aScore >= bScore && zins > bScore) {
        bScore = zins
        bPoint = 0x04
      }
      else if (aScore < bScore && zins > aScore) {
        aScore = zins
        aPoint = 0x04
      }
      if (aScore >= bScore && wins > bScore) {
        bScore = wins
        bPoint = 0x08
      }
      else if (aScore < bScore && wins > aScore) {
        aScore = wins
        aPoint = 0x08
      }
      //Now we determine the three lattice points not part of the pentachoron that may contribute.
      //This depends on the closest two pentachoron vertices, including (0,0,0,0)
      val uins: Double = 1 - inSum
      if (uins > aScore || uins > bScore) {
        //(0,0,0,0) is one of the closest two pentachoron vertices.
        val c: Byte = if (bScore > aScore) bPoint else aPoint //Our other closest vertex is the closest out of a and b.
        if ((c & 0x01) == 0) {
          xsv_ext0 = xsb - 1
          xsv_ext1 = xsb
          xsv_ext2 = xsb
          dx_ext0 = dx0 + 1
          dx_ext1 = dx0
          dx_ext2 = dx0
        }
        else {
          xsv_ext0 = xsb + 1
          xsv_ext1 = xsb + 1
          xsv_ext2 = xsb + 1
          dx_ext0 = dx0 - 1
          dx_ext1 = dx0 - 1
          dx_ext2 = dx0 - 1
        }
        if ((c & 0x02) == 0) {
          ysv_ext0 = ysb
          ysv_ext1 = ysb
          ysv_ext2 = ysb
          dy_ext0 = dy0
          dy_ext1 = dy0
          dy_ext2 = dy0
          if ((c & 0x01) == 0x01) {
            ysv_ext0 -= 1
            dy_ext0 += 1
          }
          else {
            ysv_ext1 -= 1
            dy_ext1 += 1
          }
        }
        else {
          ysv_ext0 = ysb + 1
          ysv_ext1 = ysb + 1
          ysv_ext2 = ysb + 1
          dy_ext0 = dy0 - 1
          dy_ext1 = dy0 - 1
          dy_ext2 = dy0 - 1
        }
        if ((c & 0x04) == 0) {
          zsv_ext0 = zsb
          zsv_ext1 = zsb
          zsv_ext2 = zsb
          dz_ext0 = dz0
          dz_ext1 = dz0
          dz_ext2 = dz0
          if ((c & 0x03) != 0) {
            if ((c & 0x03) == 0x03) {
              zsv_ext0 -= 1
              dz_ext0 += 1
            }
            else {
              zsv_ext1 -= 1
              dz_ext1 += 1
            }
          }
          else {
            zsv_ext2 -= 1
            dz_ext2 += 1
          }
        }
        else {
          zsv_ext0 = zsb + 1
          zsv_ext1 = zsb + 1
          zsv_ext2 = zsb + 1
          dz_ext0 = dz0 - 1
          dz_ext1 = dz0 - 1
          dz_ext2 = dz0 - 1
        }
        if ((c & 0x08) == 0) {
          wsv_ext0 = wsb
          wsv_ext1 = wsb
          wsv_ext2 = wsb - 1
          dw_ext0 = dw0
          dw_ext1 = dw0
          dw_ext2 = dw0 + 1
        }
        else {
          wsv_ext0 = wsb + 1
          wsv_ext1 = wsb + 1
          wsv_ext2 = wsb + 1
          dw_ext0 = dw0 - 1
          dw_ext1 = dw0 - 1
          dw_ext2 = dw0 - 1
        }
      }
      else {
        //(0,0,0,0) is not one of the closest two pentachoron vertices.
        val c: Byte = (aPoint | bPoint).toByte //Our three extra vertices are determined by the closest two.
        if ((c & 0x01) == 0) {
          xsv_ext0 = xsb
          xsv_ext2 = xsb
          xsv_ext1 = xsb - 1
          dx_ext0 = dx0 - 2 * SimplexNoise.SQUISH_CONSTANT_4D
          dx_ext1 = dx0 + 1 - SimplexNoise.SQUISH_CONSTANT_4D
          dx_ext2 = dx0 - SimplexNoise.SQUISH_CONSTANT_4D
        }
        else {
          xsv_ext0 = xsb + 1
          xsv_ext1 = xsb + 1
          xsv_ext2 = xsb + 1
          dx_ext0 = dx0 - 1 - 2 * SimplexNoise.SQUISH_CONSTANT_4D
          dx_ext1 = dx0 - 1 - SimplexNoise.SQUISH_CONSTANT_4D
          dx_ext2 = dx0 - 1 - SimplexNoise.SQUISH_CONSTANT_4D
        }
        if ((c & 0x02) == 0) {
          ysv_ext0 = ysb
          ysv_ext1 = ysb
          ysv_ext2 = ysb
          dy_ext0 = dy0 - 2 * SimplexNoise.SQUISH_CONSTANT_4D
          dy_ext1 = dy0 - SimplexNoise.SQUISH_CONSTANT_4D
          dy_ext2 = dy0 - SimplexNoise.SQUISH_CONSTANT_4D
          if ((c & 0x01) == 0x01) {
            ysv_ext1 -= 1
            dy_ext1 += 1
          }
          else {
            ysv_ext2 -= 1
            dy_ext2 += 1
          }
        }
        else {
          ysv_ext0 = ysb + 1
          ysv_ext1 = ysb + 1
          ysv_ext2 = ysb + 1
          dy_ext0 = dy0 - 1 - 2 * SimplexNoise.SQUISH_CONSTANT_4D
          dy_ext1 = dy0 - 1 - SimplexNoise.SQUISH_CONSTANT_4D
          dy_ext2 = dy0 - 1 - SimplexNoise.SQUISH_CONSTANT_4D
        }
        if ((c & 0x04) == 0) {
          zsv_ext0 = zsb
          zsv_ext1 = zsb
          zsv_ext2 = zsb
          dz_ext0 = dz0 - 2 * SimplexNoise.SQUISH_CONSTANT_4D
          dz_ext1 = dz0 - SimplexNoise.SQUISH_CONSTANT_4D
          dz_ext2 = dz0 - SimplexNoise.SQUISH_CONSTANT_4D
          if ((c & 0x03) == 0x03) {
            zsv_ext1 -= 1
            dz_ext1 += 1
          }
          else {
            zsv_ext2 -= 1
            dz_ext2 += 1
          }
        }
        else {
          zsv_ext2 = zsb + 1
          zsv_ext0 = zsv_ext2
          zsv_ext1 = zsv_ext2

          dz_ext0 = dz0 - 1 - 2 * SimplexNoise.SQUISH_CONSTANT_4D
          dz_ext2 = dz0 - 1 - SimplexNoise.SQUISH_CONSTANT_4D
          dz_ext1 = dz_ext2
        }
        if ((c & 0x08) == 0) {
          wsv_ext0 = wsb
          wsv_ext1 = wsb
          wsv_ext2 = wsb - 1
          dw_ext0 = dw0 - 2 * SimplexNoise.SQUISH_CONSTANT_4D
          dw_ext1 = dw0 - SimplexNoise.SQUISH_CONSTANT_4D
          dw_ext2 = dw0 + 1 - SimplexNoise.SQUISH_CONSTANT_4D
        }
        else {
          wsv_ext2 = wsb + 1
          wsv_ext0 = wsv_ext2
          wsv_ext1 = wsv_ext2
          dw_ext0 = dw0 - 1 - 2 * SimplexNoise.SQUISH_CONSTANT_4D
          dw_ext2 = dw0 - 1 - SimplexNoise.SQUISH_CONSTANT_4D
          dw_ext1 = dw_ext2
        }
      }
      //Contribution (0,0,0,0)
      var attn0: Double = 2 - dx0 * dx0 - dy0 * dy0 - dz0 * dz0 - dw0 * dw0
      if (attn0 > 0) {
        attn0 *= attn0
        value += attn0 * attn0 * extrapolate(xsb + 0, ysb + 0, zsb + 0, wsb + 0, dx0, dy0, dz0, dw0)
      }
      //Contribution (1,0,0,0)
      val dx1: Double = dx0 - 1 - SimplexNoise.SQUISH_CONSTANT_4D
      val dy1: Double = dy0 - 0 - SimplexNoise.SQUISH_CONSTANT_4D
      val dz1: Double = dz0 - 0 - SimplexNoise.SQUISH_CONSTANT_4D
      val dw1: Double = dw0 - 0 - SimplexNoise.SQUISH_CONSTANT_4D
      var attn1: Double = 2 - dx1 * dx1 - dy1 * dy1 - dz1 * dz1 - dw1 * dw1
      if (attn1 > 0) {
        attn1 *= attn1
        value += attn1 * attn1 * extrapolate(xsb + 1, ysb + 0, zsb + 0, wsb + 0, dx1, dy1, dz1, dw1)
      }
      //Contribution (0,1,0,0)
      val dx2: Double = dx0 - 0 - SimplexNoise.SQUISH_CONSTANT_4D
      val dy2: Double = dy0 - 1 - SimplexNoise.SQUISH_CONSTANT_4D
      val dz2: Double = dz1
      val dw2: Double = dw1
      var attn2: Double = 2 - dx2 * dx2 - dy2 * dy2 - dz2 * dz2 - dw2 * dw2
      if (attn2 > 0) {
        attn2 *= attn2
        value += attn2 * attn2 * extrapolate(xsb + 0, ysb + 1, zsb + 0, wsb + 0, dx2, dy2, dz2, dw2)
      }
      //Contribution (0,0,1,0)
      val dx3: Double = dx2
      val dy3: Double = dy1
      val dz3: Double = dz0 - 1 - SimplexNoise.SQUISH_CONSTANT_4D
      val dw3: Double = dw1
      var attn3: Double = 2 - dx3 * dx3 - dy3 * dy3 - dz3 * dz3 - dw3 * dw3
      if (attn3 > 0) {
        attn3 *= attn3
        value += attn3 * attn3 * extrapolate(xsb + 0, ysb + 0, zsb + 1, wsb + 0, dx3, dy3, dz3, dw3)
      }
      //Contribution (0,0,0,1)
      val dx4: Double = dx2
      val dy4: Double = dy1
      val dz4: Double = dz1
      val dw4: Double = dw0 - 1 - SimplexNoise.SQUISH_CONSTANT_4D
      var attn4: Double = 2 - dx4 * dx4 - dy4 * dy4 - dz4 * dz4 - dw4 * dw4
      if (attn4 > 0) {
        attn4 *= attn4
        value += attn4 * attn4 * extrapolate(xsb + 0, ysb + 0, zsb + 0, wsb + 1, dx4, dy4, dz4, dw4)
      }
    }
    else if (inSum >= 3) {
      //We're inside the pentachoron (4-Simplex) at (1,1,1,1)
      //Determine which two of (1,1,1,0), (1,1,0,1), (1,0,1,1), (0,1,1,1) are closest.
      var aPoint: Byte = 0x0E
      var aScore: Double = xins
      var bPoint: Byte = 0x0D
      var bScore: Double = yins
      if (aScore <= bScore && zins < bScore) {
        bScore = zins
        bPoint = 0x0B
      }
      else if (aScore > bScore && zins < aScore) {
        aScore = zins
        aPoint = 0x0B
      }
      if (aScore <= bScore && wins < bScore) {
        bScore = wins
        bPoint = 0x07
      }
      else if (aScore > bScore && wins < aScore) {
        aScore = wins
        aPoint = 0x07
      }
      //Now we determine the three lattice points not part of the pentachoron that may contribute.
      //This depends on the closest two pentachoron vertices, including (0,0,0,0)
      val uins: Double = 4 - inSum
      if (uins < aScore || uins < bScore) {
        //(1,1,1,1) is one of the closest two pentachoron vertices.
        val c: Byte = if (bScore < aScore) bPoint else aPoint //Our other closest vertex is the closest out of a and b.
        if ((c & 0x01) != 0) {
          xsv_ext0 = xsb + 2
          xsv_ext2 = xsb + 1
          xsv_ext1 = xsv_ext2
          dx_ext0 = dx0 - 2 - 4 * SimplexNoise.SQUISH_CONSTANT_4D
          dx_ext1 = dx0 - 1 - 4 * SimplexNoise.SQUISH_CONSTANT_4D
          dx_ext2 = dx_ext1
        }
        else {
          xsv_ext2 = xsb
          xsv_ext0 = xsb
          xsv_ext1 = xsb

          dx_ext2 = dx0 - 4 * SimplexNoise.SQUISH_CONSTANT_4D
          dx_ext0 = dx_ext2
          dx_ext1 = dx_ext2
        }
        if ((c & 0x02) != 0) {
          ysv_ext2 = ysb + 1
          ysv_ext0 = ysv_ext2
          ysv_ext1 = ysv_ext2
          dy_ext2 = dy0 - 1 - 4 * SimplexNoise.SQUISH_CONSTANT_4D
          dy_ext0 = dy_ext2
          dy_ext1 = dy_ext2
          if ((c & 0x01) != 0) {
            ysv_ext1 += 1
            dy_ext1 -= 1
          }
          else {
            ysv_ext0 += 1
            dy_ext0 -= 1
          }
        }
        else {
          ysv_ext2 = ysb
          ysv_ext0 = ysb
          ysv_ext1 = ysb

          dy_ext2 = dy0 - 4 * SimplexNoise.SQUISH_CONSTANT_4D
          dy_ext0 = dy_ext2
          dy_ext1 = dy_ext2
        }
        if ((c & 0x04) != 0) {
          zsv_ext2 = zsb + 1
          zsv_ext0 = zsv_ext2
          zsv_ext1 = zsv_ext2

          dz_ext2 = dz0 - 1 - 4 * SimplexNoise.SQUISH_CONSTANT_4D
          dz_ext0 = dz_ext2
          dz_ext1 = dz_ext2
          if ((c & 0x03) != 0x03) {
            if ((c & 0x03) == 0) {
              zsv_ext0 += 1
              dz_ext0 -= 1
            }
            else {
              zsv_ext1 += 1
              dz_ext1 -= 1
            }
          }
          else {
            zsv_ext2 += 1
            dz_ext2 -= 1
          }
        }
        else {
          zsv_ext0 = zsb
          zsv_ext1 = zsb
          zsv_ext2 = zsb
          dz_ext2 = dz0 - 4 * SimplexNoise.SQUISH_CONSTANT_4D
          dz_ext0 = dz_ext2
          dz_ext1 = dz_ext2
        }
        if ((c & 0x08) != 0) {
          wsv_ext1 = wsb + 1
          wsv_ext0 = wsv_ext1
          wsv_ext2 = wsb + 2
          dw_ext1 = dw0 - 1 - 4 * SimplexNoise.SQUISH_CONSTANT_4D
          dw_ext0 = dw_ext1
          dw_ext2 = dw0 - 2 - 4 * SimplexNoise.SQUISH_CONSTANT_4D
        }
        else {
          wsv_ext0 = wsb
          wsv_ext1 = wsb
          wsv_ext2 = wsb
          dw_ext2 = dw0 - 4 * SimplexNoise.SQUISH_CONSTANT_4D
          dw_ext0 = dw_ext2
          dw_ext1 = dw_ext2
        }
      }
      else {
        //(1,1,1,1) is not one of the closest two pentachoron vertices.
        val c: Byte = (aPoint & bPoint).toByte //Our three extra vertices are determined by the closest two.
        if ((c & 0x01) != 0) {
          xsv_ext0 = xsb + 1
          xsv_ext2 = xsb + 1
          xsv_ext1 = xsb + 2
          dx_ext0 = dx0 - 1 - 2 * SimplexNoise.SQUISH_CONSTANT_4D
          dx_ext1 = dx0 - 2 - 3 * SimplexNoise.SQUISH_CONSTANT_4D
          dx_ext2 = dx0 - 1 - 3 * SimplexNoise.SQUISH_CONSTANT_4D
        }
        else {
          xsv_ext0 = xsb
          xsv_ext1 = xsb
          xsv_ext2 = xsb
          dx_ext0 = dx0 - 2 * SimplexNoise.SQUISH_CONSTANT_4D
          dx_ext2 = dx0 - 3 * SimplexNoise.SQUISH_CONSTANT_4D
          dx_ext1 = dx_ext2
        }
        if ((c & 0x02) != 0) {
          ysv_ext2 = ysb + 1
          ysv_ext0 = ysv_ext2
          ysv_ext1 = ysv_ext2
          dy_ext0 = dy0 - 1 - 2 * SimplexNoise.SQUISH_CONSTANT_4D
          dy_ext2 = dy0 - 1 - 3 * SimplexNoise.SQUISH_CONSTANT_4D
          dy_ext1 = dy_ext2
          if ((c & 0x01) != 0) {
            ysv_ext2 += 1
            dy_ext2 -= 1
          }
          else {
            ysv_ext1 += 1
            dy_ext1 -= 1
          }
        }
        else {
          ysv_ext2 = ysb
          ysv_ext0 = ysb
          ysv_ext1 = ysb
          dy_ext0 = dy0 - 2 * SimplexNoise.SQUISH_CONSTANT_4D
          dy_ext2 = dy0 - 3 * SimplexNoise.SQUISH_CONSTANT_4D
          dy_ext1 = dy_ext2
        }
        if ((c & 0x04) != 0) {
          zsv_ext2 = zsb + 1
          zsv_ext0 = zsv_ext2
          zsv_ext1 = zsv_ext2
          dz_ext0 = dz0 - 1 - 2 * SimplexNoise.SQUISH_CONSTANT_4D
          dz_ext2 = dz0 - 1 - 3 * SimplexNoise.SQUISH_CONSTANT_4D
          dz_ext1 = dz_ext2
          if ((c & 0x03) != 0) {
            zsv_ext2 += 1
            dz_ext2 -= 1
          }
          else {
            zsv_ext1 += 1
            dz_ext1 -= 1
          }
        }
        else {
          zsv_ext2 = zsb
          zsv_ext0 = zsb
          zsv_ext1 = zsb
          dz_ext0 = dz0 - 2 * SimplexNoise.SQUISH_CONSTANT_4D
          dz_ext2 = dz0 - 3 * SimplexNoise.SQUISH_CONSTANT_4D
          dz_ext1 = dz_ext2
        }
        if ((c & 0x08) != 0) {
          wsv_ext1 = wsb + 1
          wsv_ext0 = wsv_ext1
          wsv_ext2 = wsb + 2
          dw_ext0 = dw0 - 1 - 2 * SimplexNoise.SQUISH_CONSTANT_4D
          dw_ext1 = dw0 - 1 - 3 * SimplexNoise.SQUISH_CONSTANT_4D
          dw_ext2 = dw0 - 2 - 3 * SimplexNoise.SQUISH_CONSTANT_4D
        }
        else {
          wsv_ext2 = wsb
          wsv_ext0 = wsb
          wsv_ext1 = wsb
          dw_ext0 = dw0 - 2 * SimplexNoise.SQUISH_CONSTANT_4D
          dw_ext2 = dw0 - 3 * SimplexNoise.SQUISH_CONSTANT_4D
          dw_ext1 = dw_ext2
        }
      }
      //Contribution (1,1,1,0)
      val dx4: Double = dx0 - 1 - 3 * SimplexNoise.SQUISH_CONSTANT_4D
      val dy4: Double = dy0 - 1 - 3 * SimplexNoise.SQUISH_CONSTANT_4D
      val dz4: Double = dz0 - 1 - 3 * SimplexNoise.SQUISH_CONSTANT_4D
      val dw4: Double = dw0 - 3 * SimplexNoise.SQUISH_CONSTANT_4D
      var attn4: Double = 2 - dx4 * dx4 - dy4 * dy4 - dz4 * dz4 - dw4 * dw4
      if (attn4 > 0) {
        attn4 *= attn4
        value += attn4 * attn4 * extrapolate(xsb + 1, ysb + 1, zsb + 1, wsb + 0, dx4, dy4, dz4, dw4)
      }
      //Contribution (1,1,0,1)
      val dx3: Double = dx4
      val dy3: Double = dy4
      val dz3: Double = dz0 - 3 * SimplexNoise.SQUISH_CONSTANT_4D
      val dw3: Double = dw0 - 1 - 3 * SimplexNoise.SQUISH_CONSTANT_4D
      var attn3: Double = 2 - dx3 * dx3 - dy3 * dy3 - dz3 * dz3 - dw3 * dw3
      if (attn3 > 0) {
        attn3 *= attn3
        value += attn3 * attn3 * extrapolate(xsb + 1, ysb + 1, zsb + 0, wsb + 1, dx3, dy3, dz3, dw3)
      }
      //Contribution (1,0,1,1)
      val dx2: Double = dx4
      val dy2: Double = dy0 - 3 * SimplexNoise.SQUISH_CONSTANT_4D
      val dz2: Double = dz4
      val dw2: Double = dw3
      var attn2: Double = 2 - dx2 * dx2 - dy2 * dy2 - dz2 * dz2 - dw2 * dw2
      if (attn2 > 0) {
        attn2 *= attn2
        value += attn2 * attn2 * extrapolate(xsb + 1, ysb + 0, zsb + 1, wsb + 1, dx2, dy2, dz2, dw2)
      }
      //Contribution (0,1,1,1)
      val dx1: Double = dx0 - 3 * SimplexNoise.SQUISH_CONSTANT_4D
      val dz1: Double = dz4
      val dy1: Double = dy4
      val dw1: Double = dw3
      var attn1: Double = 2 - dx1 * dx1 - dy1 * dy1 - dz1 * dz1 - dw1 * dw1
      if (attn1 > 0) {
        attn1 *= attn1
        value += attn1 * attn1 * extrapolate(xsb + 0, ysb + 1, zsb + 1, wsb + 1, dx1, dy1, dz1, dw1)
      }
      //Contribution (1,1,1,1)
      dx0 = dx0 - 1 - 4 * SimplexNoise.SQUISH_CONSTANT_4D
      dy0 = dy0 - 1 - 4 * SimplexNoise.SQUISH_CONSTANT_4D
      dz0 = dz0 - 1 - 4 * SimplexNoise.SQUISH_CONSTANT_4D
      dw0 = dw0 - 1 - 4 * SimplexNoise.SQUISH_CONSTANT_4D
      var attn0: Double = 2 - dx0 * dx0 - dy0 * dy0 - dz0 * dz0 - dw0 * dw0
      if (attn0 > 0) {
        attn0 *= attn0
        value += attn0 * attn0 * extrapolate(xsb + 1, ysb + 1, zsb + 1, wsb + 1, dx0, dy0, dz0, dw0)
      }
    }
    else if (inSum <= 2) {
      //We're inside the first dispentachoron (Rectified 4-Simplex)
      var aScore: Double = .0
      var aPoint: Byte = 0
      var aIsBiggerSide: Boolean = true
      var bScore: Double = .0
      var bPoint: Byte = 0
      var bIsBiggerSide: Boolean = true
      //Decide between (1,1,0,0) and (0,0,1,1)
      if (xins + yins > zins + wins) {
        aScore = xins + yins
        aPoint = 0x03
      }
      else {
        aScore = zins + wins
        aPoint = 0x0C
      }
      //Decide between (1,0,1,0) and (0,1,0,1)
      if (xins + zins > yins + wins) {
        bScore = xins + zins
        bPoint = 0x05
      }
      else {
        bScore = yins + wins
        bPoint = 0x0A
      }
      //Closer between (1,0,0,1) and (0,1,1,0) will replace the further of a and b, if closer.
      if (xins + wins > yins + zins) {
        val score: Double = xins + wins
        if (aScore >= bScore && score > bScore) {
          bScore = score
          bPoint = 0x09
        }
        else if (aScore < bScore && score > aScore) {
          aScore = score
          aPoint = 0x09
        }
      }
      else {
        val score: Double = yins + zins
        if (aScore >= bScore && score > bScore) {
          bScore = score
          bPoint = 0x06
        }
        else if (aScore < bScore && score > aScore) {
          aScore = score
          aPoint = 0x06
        }
      }
      //Decide if (1,0,0,0) is closer.
      val p1: Double = 2 - inSum + xins
      if (aScore >= bScore && p1 > bScore) {
        bScore = p1
        bPoint = 0x01
        bIsBiggerSide = false
      }
      else if (aScore < bScore && p1 > aScore) {
        aScore = p1
        aPoint = 0x01
        aIsBiggerSide = false
      }
      //Decide if (0,1,0,0) is closer.
      val p2: Double = 2 - inSum + yins
      if (aScore >= bScore && p2 > bScore) {
        bScore = p2
        bPoint = 0x02
        bIsBiggerSide = false
      }
      else if (aScore < bScore && p2 > aScore) {
        aScore = p2
        aPoint = 0x02
        aIsBiggerSide = false
      }
      //Decide if (0,0,1,0) is closer.
      val p3: Double = 2 - inSum + zins
      if (aScore >= bScore && p3 > bScore) {
        bScore = p3
        bPoint = 0x04
        bIsBiggerSide = false
      }
      else if (aScore < bScore && p3 > aScore) {
        aScore = p3
        aPoint = 0x04
        aIsBiggerSide = false
      }
      //Decide if (0,0,0,1) is closer.
      val p4: Double = 2 - inSum + wins
      if (aScore >= bScore && p4 > bScore) {
        bScore = p4
        bPoint = 0x08
        bIsBiggerSide = false
      }
      else if (aScore < bScore && p4 > aScore) {
        aScore = p4
        aPoint = 0x08
        aIsBiggerSide = false
      }
      //Where each of the two closest points are determines how the extra three vertices are calculated.
      if (aIsBiggerSide == bIsBiggerSide) {
        if (aIsBiggerSide) {
          //Both closest points on the bigger side
          val c1: Byte = (aPoint | bPoint).toByte
          val c2: Byte = (aPoint & bPoint).toByte
          if ((c1 & 0x01) == 0) {
            xsv_ext0 = xsb
            xsv_ext1 = xsb - 1
            dx_ext0 = dx0 - 3 * SimplexNoise.SQUISH_CONSTANT_4D
            dx_ext1 = dx0 + 1 - 2 * SimplexNoise.SQUISH_CONSTANT_4D
          }
          else {
            xsv_ext1 = xsb + 1
            xsv_ext0 = xsv_ext1
            dx_ext0 = dx0 - 1 - 3 * SimplexNoise.SQUISH_CONSTANT_4D
            dx_ext1 = dx0 - 1 - 2 * SimplexNoise.SQUISH_CONSTANT_4D
          }
          if ((c1 & 0x02) == 0) {
            ysv_ext0 = ysb
            ysv_ext1 = ysb - 1
            dy_ext0 = dy0 - 3 * SimplexNoise.SQUISH_CONSTANT_4D
            dy_ext1 = dy0 + 1 - 2 * SimplexNoise.SQUISH_CONSTANT_4D
          }
          else {
            ysv_ext1 = ysb + 1
            ysv_ext0 = ysv_ext1
            dy_ext0 = dy0 - 1 - 3 * SimplexNoise.SQUISH_CONSTANT_4D
            dy_ext1 = dy0 - 1 - 2 * SimplexNoise.SQUISH_CONSTANT_4D
          }
          if ((c1 & 0x04) == 0) {
            zsv_ext0 = zsb
            zsv_ext1 = zsb - 1
            dz_ext0 = dz0 - 3 * SimplexNoise.SQUISH_CONSTANT_4D
            dz_ext1 = dz0 + 1 - 2 * SimplexNoise.SQUISH_CONSTANT_4D
          }
          else {
            zsv_ext1 = zsb + 1
            zsv_ext0 = zsv_ext1
            dz_ext0 = dz0 - 1 - 3 * SimplexNoise.SQUISH_CONSTANT_4D
            dz_ext1 = dz0 - 1 - 2 * SimplexNoise.SQUISH_CONSTANT_4D
          }
          if ((c1 & 0x08) == 0) {
            wsv_ext0 = wsb
            wsv_ext1 = wsb - 1
            dw_ext0 = dw0 - 3 * SimplexNoise.SQUISH_CONSTANT_4D
            dw_ext1 = dw0 + 1 - 2 * SimplexNoise.SQUISH_CONSTANT_4D
          }
          else {
            wsv_ext1 = wsb + 1
            wsv_ext0 = wsv_ext1
            dw_ext0 = dw0 - 1 - 3 * SimplexNoise.SQUISH_CONSTANT_4D
            dw_ext1 = dw0 - 1 - 2 * SimplexNoise.SQUISH_CONSTANT_4D
          }
          //One combination is a permutation of (0,0,0,2) based on c2
          xsv_ext2 = xsb
          ysv_ext2 = ysb
          zsv_ext2 = zsb
          wsv_ext2 = wsb
          dx_ext2 = dx0 - 2 * SimplexNoise.SQUISH_CONSTANT_4D
          dy_ext2 = dy0 - 2 * SimplexNoise.SQUISH_CONSTANT_4D
          dz_ext2 = dz0 - 2 * SimplexNoise.SQUISH_CONSTANT_4D
          dw_ext2 = dw0 - 2 * SimplexNoise.SQUISH_CONSTANT_4D
          if ((c2 & 0x01) != 0) {
            xsv_ext2 += 2
            dx_ext2 -= 2
          }
          else if ((c2 & 0x02) != 0) {
            ysv_ext2 += 2
            dy_ext2 -= 2
          }
          else if ((c2 & 0x04) != 0) {
            zsv_ext2 += 2
            dz_ext2 -= 2
          }
          else {
            wsv_ext2 += 2
            dw_ext2 -= 2
          }
        }
        else {
          //Both closest points on the smaller side
          //One of the two extra points is (0,0,0,0)
          xsv_ext2 = xsb
          ysv_ext2 = ysb
          zsv_ext2 = zsb
          wsv_ext2 = wsb
          dx_ext2 = dx0
          dy_ext2 = dy0
          dz_ext2 = dz0
          dw_ext2 = dw0
          //Other two points are based on the omitted axes.
          val c: Byte = (aPoint | bPoint).toByte
          if ((c & 0x01) == 0) {
            xsv_ext0 = xsb - 1
            xsv_ext1 = xsb
            dx_ext0 = dx0 + 1 - SimplexNoise.SQUISH_CONSTANT_4D
            dx_ext1 = dx0 - SimplexNoise.SQUISH_CONSTANT_4D
          }
          else {
            xsv_ext1 = xsb + 1
            xsv_ext0 = xsv_ext1
            dx_ext1 = dx0 - 1 - SimplexNoise.SQUISH_CONSTANT_4D
            dx_ext0 = dx_ext1
          }
          if ((c & 0x02) == 0) {
            ysv_ext0 = ysb
            ysv_ext1 = ysb
            dy_ext0 = dy0 - SimplexNoise.SQUISH_CONSTANT_4D
            dy_ext1 = dy0 - SimplexNoise.SQUISH_CONSTANT_4D
            if ((c & 0x01) == 0x01) {
              ysv_ext0 -= 1
              dy_ext0 += 1
            }
            else {
              ysv_ext1 -= 1
              dy_ext1 += 1
            }
          }
          else {
            ysv_ext1 = ysb + 1
            ysv_ext0 = ysv_ext1
            dy_ext1 = dy0 - 1 - SimplexNoise.SQUISH_CONSTANT_4D
            dy_ext0 = dy_ext1
          }
          if ((c & 0x04) == 0) {
            zsv_ext1 = zsb
            zsv_ext0 = zsb
            dz_ext1 = dz0 - SimplexNoise.SQUISH_CONSTANT_4D
            dz_ext0 = dz_ext1
            if ((c & 0x03) == 0x03) {
              zsv_ext0 -= 1
              dz_ext0 += 1
            }
            else {
              zsv_ext1 -= 1
              dz_ext1 += 1
            }
          }
          else {
            zsv_ext1 = zsb + 1
            zsv_ext0 = zsv_ext1
            dz_ext1 = dz0 - 1 - SimplexNoise.SQUISH_CONSTANT_4D
            dz_ext0 = dz_ext1
          }
          if ((c & 0x08) == 0) {
            wsv_ext0 = wsb
            wsv_ext1 = wsb - 1
            dw_ext0 = dw0 - SimplexNoise.SQUISH_CONSTANT_4D
            dw_ext1 = dw0 + 1 - SimplexNoise.SQUISH_CONSTANT_4D
          }
          else {
            wsv_ext1 = wsb + 1
            wsv_ext0 = wsv_ext1
            dw_ext1 = dw0 - 1 - SimplexNoise.SQUISH_CONSTANT_4D
            dw_ext0 = dw_ext1
          }
        }
      }
      else {
        //One point on each "side"
        var c1: Byte = 0
        var c2: Byte = 0
        if (aIsBiggerSide) {
          c1 = aPoint
          c2 = bPoint
        }
        else {
          c1 = bPoint
          c2 = aPoint
        }
        //Two contributions are the bigger-sided point with each 0 replaced with -1.
        if ((c1 & 0x01) == 0) {
          xsv_ext0 = xsb - 1
          xsv_ext1 = xsb
          dx_ext0 = dx0 + 1 - SimplexNoise.SQUISH_CONSTANT_4D
          dx_ext1 = dx0 - SimplexNoise.SQUISH_CONSTANT_4D
        }
        else {
          xsv_ext1 = xsb + 1
          xsv_ext0 = xsv_ext1
          dx_ext1 = dx0 - 1 - SimplexNoise.SQUISH_CONSTANT_4D
          dx_ext0 = dx_ext1
        }
        if ((c1 & 0x02) == 0) {
          ysv_ext1 = ysb
          ysv_ext0 = ysb
          dy_ext1 = dy0 - SimplexNoise.SQUISH_CONSTANT_4D
          dy_ext0 = dy_ext1
          if ((c1 & 0x01) == 0x01) {
            ysv_ext0 -= 1
            dy_ext0 += 1
          }
          else {
            ysv_ext1 -= 1
            dy_ext1 += 1
          }
        }
        else {
          ysv_ext1 = ysb + 1
          ysv_ext0 = ysv_ext1
          dy_ext1 = dy0 - 1 - SimplexNoise.SQUISH_CONSTANT_4D
          dy_ext0 = dy_ext1
        }
        if ((c1 & 0x04) == 0) {
          zsv_ext1 = zsb
          zsv_ext0 = zsb
          dz_ext1 = dz0 - SimplexNoise.SQUISH_CONSTANT_4D
          dz_ext0 = dz_ext1
          if ((c1 & 0x03) == 0x03) {
            zsv_ext0 -= 1
            dz_ext0 += 1
          }
          else {
            zsv_ext1 -= 1
            dz_ext1 += 1
          }
        }
        else {
          zsv_ext1 = zsb + 1
          zsv_ext0 = zsv_ext1
          dz_ext1 = dz0 - 1 - SimplexNoise.SQUISH_CONSTANT_4D
          dz_ext0 = dz_ext1
        }
        if ((c1 & 0x08) == 0) {
          wsv_ext0 = wsb
          wsv_ext1 = wsb - 1
          dw_ext0 = dw0 - SimplexNoise.SQUISH_CONSTANT_4D
          dw_ext1 = dw0 + 1 - SimplexNoise.SQUISH_CONSTANT_4D
        }
        else {
          wsv_ext1 = wsb + 1
          wsv_ext0 = wsv_ext1
          dw_ext1 = dw0 - 1 - SimplexNoise.SQUISH_CONSTANT_4D
          dw_ext0 = dw_ext1
        }
        //One contribution is a permutation of (0,0,0,2) based on the smaller-sided point
        xsv_ext2 = xsb
        ysv_ext2 = ysb
        zsv_ext2 = zsb
        wsv_ext2 = wsb
        dx_ext2 = dx0 - 2 * SimplexNoise.SQUISH_CONSTANT_4D
        dy_ext2 = dy0 - 2 * SimplexNoise.SQUISH_CONSTANT_4D
        dz_ext2 = dz0 - 2 * SimplexNoise.SQUISH_CONSTANT_4D
        dw_ext2 = dw0 - 2 * SimplexNoise.SQUISH_CONSTANT_4D
        if ((c2 & 0x01) != 0) {
          xsv_ext2 += 2
          dx_ext2 -= 2
        }
        else if ((c2 & 0x02) != 0) {
          ysv_ext2 += 2
          dy_ext2 -= 2
        }
        else if ((c2 & 0x04) != 0) {
          zsv_ext2 += 2
          dz_ext2 -= 2
        }
        else {
          wsv_ext2 += 2
          dw_ext2 -= 2
        }
      }
      //Contribution (1,0,0,0)
      val dx1: Double = dx0 - 1 - SimplexNoise.SQUISH_CONSTANT_4D
      val dy1: Double = dy0 - 0 - SimplexNoise.SQUISH_CONSTANT_4D
      val dz1: Double = dz0 - 0 - SimplexNoise.SQUISH_CONSTANT_4D
      val dw1: Double = dw0 - 0 - SimplexNoise.SQUISH_CONSTANT_4D
      var attn1: Double = 2 - dx1 * dx1 - dy1 * dy1 - dz1 * dz1 - dw1 * dw1
      if (attn1 > 0) {
        attn1 *= attn1
        value += attn1 * attn1 * extrapolate(xsb + 1, ysb + 0, zsb + 0, wsb + 0, dx1, dy1, dz1, dw1)
      }
      //Contribution (0,1,0,0)
      val dx2: Double = dx0 - 0 - SimplexNoise.SQUISH_CONSTANT_4D
      val dy2: Double = dy0 - 1 - SimplexNoise.SQUISH_CONSTANT_4D
      val dz2: Double = dz1
      val dw2: Double = dw1
      var attn2: Double = 2 - dx2 * dx2 - dy2 * dy2 - dz2 * dz2 - dw2 * dw2
      if (attn2 > 0) {
        attn2 *= attn2
        value += attn2 * attn2 * extrapolate(xsb + 0, ysb + 1, zsb + 0, wsb + 0, dx2, dy2, dz2, dw2)
      }
      //Contribution (0,0,1,0)
      val dx3: Double = dx2
      val dy3: Double = dy1
      val dz3: Double = dz0 - 1 - SimplexNoise.SQUISH_CONSTANT_4D
      val dw3: Double = dw1
      var attn3: Double = 2 - dx3 * dx3 - dy3 * dy3 - dz3 * dz3 - dw3 * dw3
      if (attn3 > 0) {
        attn3 *= attn3
        value += attn3 * attn3 * extrapolate(xsb + 0, ysb + 0, zsb + 1, wsb + 0, dx3, dy3, dz3, dw3)
      }
      //Contribution (0,0,0,1)
      val dx4: Double = dx2
      val dy4: Double = dy1
      val dz4: Double = dz1
      val dw4: Double = dw0 - 1 - SimplexNoise.SQUISH_CONSTANT_4D
      var attn4: Double = 2 - dx4 * dx4 - dy4 * dy4 - dz4 * dz4 - dw4 * dw4
      if (attn4 > 0) {
        attn4 *= attn4
        value += attn4 * attn4 * extrapolate(xsb + 0, ysb + 0, zsb + 0, wsb + 1, dx4, dy4, dz4, dw4)
      }
      //Contribution (1,1,0,0)
      val dx5: Double = dx0 - 1 - 2 * SimplexNoise.SQUISH_CONSTANT_4D
      val dy5: Double = dy0 - 1 - 2 * SimplexNoise.SQUISH_CONSTANT_4D
      val dz5: Double = dz0 - 0 - 2 * SimplexNoise.SQUISH_CONSTANT_4D
      val dw5: Double = dw0 - 0 - 2 * SimplexNoise.SQUISH_CONSTANT_4D
      var attn5: Double = 2 - dx5 * dx5 - dy5 * dy5 - dz5 * dz5 - dw5 * dw5
      if (attn5 > 0) {
        attn5 *= attn5
        value += attn5 * attn5 * extrapolate(xsb + 1, ysb + 1, zsb + 0, wsb + 0, dx5, dy5, dz5, dw5)
      }
      //Contribution (1,0,1,0)
      val dx6: Double = dx0 - 1 - 2 * SimplexNoise.SQUISH_CONSTANT_4D
      val dy6: Double = dy0 - 0 - 2 * SimplexNoise.SQUISH_CONSTANT_4D
      val dz6: Double = dz0 - 1 - 2 * SimplexNoise.SQUISH_CONSTANT_4D
      val dw6: Double = dw0 - 0 - 2 * SimplexNoise.SQUISH_CONSTANT_4D
      var attn6: Double = 2 - dx6 * dx6 - dy6 * dy6 - dz6 * dz6 - dw6 * dw6
      if (attn6 > 0) {
        attn6 *= attn6
        value += attn6 * attn6 * extrapolate(xsb + 1, ysb + 0, zsb + 1, wsb + 0, dx6, dy6, dz6, dw6)
      }
      //Contribution (1,0,0,1)
      val dx7: Double = dx0 - 1 - 2 * SimplexNoise.SQUISH_CONSTANT_4D
      val dy7: Double = dy0 - 0 - 2 * SimplexNoise.SQUISH_CONSTANT_4D
      val dz7: Double = dz0 - 0 - 2 * SimplexNoise.SQUISH_CONSTANT_4D
      val dw7: Double = dw0 - 1 - 2 * SimplexNoise.SQUISH_CONSTANT_4D
      var attn7: Double = 2 - dx7 * dx7 - dy7 * dy7 - dz7 * dz7 - dw7 * dw7
      if (attn7 > 0) {
        attn7 *= attn7
        value += attn7 * attn7 * extrapolate(xsb + 1, ysb + 0, zsb + 0, wsb + 1, dx7, dy7, dz7, dw7)
      }
      //Contribution (0,1,1,0)
      val dx8: Double = dx0 - 0 - 2 * SimplexNoise.SQUISH_CONSTANT_4D
      val dy8: Double = dy0 - 1 - 2 * SimplexNoise.SQUISH_CONSTANT_4D
      val dz8: Double = dz0 - 1 - 2 * SimplexNoise.SQUISH_CONSTANT_4D
      val dw8: Double = dw0 - 0 - 2 * SimplexNoise.SQUISH_CONSTANT_4D
      var attn8: Double = 2 - dx8 * dx8 - dy8 * dy8 - dz8 * dz8 - dw8 * dw8
      if (attn8 > 0) {
        attn8 *= attn8
        value += attn8 * attn8 * extrapolate(xsb + 0, ysb + 1, zsb + 1, wsb + 0, dx8, dy8, dz8, dw8)
      }
      //Contribution (0,1,0,1)
      val dx9: Double = dx0 - 0 - 2 * SimplexNoise.SQUISH_CONSTANT_4D
      val dy9: Double = dy0 - 1 - 2 * SimplexNoise.SQUISH_CONSTANT_4D
      val dz9: Double = dz0 - 0 - 2 * SimplexNoise.SQUISH_CONSTANT_4D
      val dw9: Double = dw0 - 1 - 2 * SimplexNoise.SQUISH_CONSTANT_4D
      var attn9: Double = 2 - dx9 * dx9 - dy9 * dy9 - dz9 * dz9 - dw9 * dw9
      if (attn9 > 0) {
        attn9 *= attn9
        value += attn9 * attn9 * extrapolate(xsb + 0, ysb + 1, zsb + 0, wsb + 1, dx9, dy9, dz9, dw9)
      }
      //Contribution (0,0,1,1)
      val dx10: Double = dx0 - 0 - 2 * SimplexNoise.SQUISH_CONSTANT_4D
      val dy10: Double = dy0 - 0 - 2 * SimplexNoise.SQUISH_CONSTANT_4D
      val dz10: Double = dz0 - 1 - 2 * SimplexNoise.SQUISH_CONSTANT_4D
      val dw10: Double = dw0 - 1 - 2 * SimplexNoise.SQUISH_CONSTANT_4D
      var attn10: Double = 2 - dx10 * dx10 - dy10 * dy10 - dz10 * dz10 - dw10 * dw10
      if (attn10 > 0) {
        attn10 *= attn10
        value += attn10 * attn10 * extrapolate(xsb + 0, ysb + 0, zsb + 1, wsb + 1, dx10, dy10, dz10, dw10)
      }
    }
    else {
      //We're inside the second dispentachoron (Rectified 4-Simplex)
      var aScore: Double = .0
      var aPoint: Byte = 0
      var aIsBiggerSide: Boolean = true
      var bScore: Double = .0
      var bPoint: Byte = 0
      var bIsBiggerSide: Boolean = true
      //Decide between (0,0,1,1) and (1,1,0,0)
      if (xins + yins < zins + wins) {
        aScore = xins + yins
        aPoint = 0x0C
      }
      else {
        aScore = zins + wins
        aPoint = 0x03
      }
      //Decide between (0,1,0,1) and (1,0,1,0)
      if (xins + zins < yins + wins) {
        bScore = xins + zins
        bPoint = 0x0A
      }
      else {
        bScore = yins + wins
        bPoint = 0x05
      }
      //Closer between (0,1,1,0) and (1,0,0,1) will replace the further of a and b, if closer.
      if (xins + wins < yins + zins) {
        val score: Double = xins + wins
        if (aScore <= bScore && score < bScore) {
          bScore = score
          bPoint = 0x06
        }
        else if (aScore > bScore && score < aScore) {
          aScore = score
          aPoint = 0x06
        }
      }
      else {
        val score: Double = yins + zins
        if (aScore <= bScore && score < bScore) {
          bScore = score
          bPoint = 0x09
        }
        else if (aScore > bScore && score < aScore) {
          aScore = score
          aPoint = 0x09
        }
      }
      //Decide if (0,1,1,1) is closer.
      val p1: Double = 3 - inSum + xins
      if (aScore <= bScore && p1 < bScore) {
        bScore = p1
        bPoint = 0x0E
        bIsBiggerSide = false
      }
      else if (aScore > bScore && p1 < aScore) {
        aScore = p1
        aPoint = 0x0E
        aIsBiggerSide = false
      }
      //Decide if (1,0,1,1) is closer.
      val p2: Double = 3 - inSum + yins
      if (aScore <= bScore && p2 < bScore) {
        bScore = p2
        bPoint = 0x0D
        bIsBiggerSide = false
      }
      else if (aScore > bScore && p2 < aScore) {
        aScore = p2
        aPoint = 0x0D
        aIsBiggerSide = false
      }
      //Decide if (1,1,0,1) is closer.
      val p3: Double = 3 - inSum + zins
      if (aScore <= bScore && p3 < bScore) {
        bScore = p3
        bPoint = 0x0B
        bIsBiggerSide = false
      }
      else if (aScore > bScore && p3 < aScore) {
        aScore = p3
        aPoint = 0x0B
        aIsBiggerSide = false
      }
      //Decide if (1,1,1,0) is closer.
      val p4: Double = 3 - inSum + wins
      if (aScore <= bScore && p4 < bScore) {
        bScore = p4
        bPoint = 0x07
        bIsBiggerSide = false
      }
      else if (aScore > bScore && p4 < aScore) {
        aScore = p4
        aPoint = 0x07
        aIsBiggerSide = false
      }
      //Where each of the two closest points are determines how the extra three vertices are calculated.
      if (aIsBiggerSide == bIsBiggerSide) {
        if (aIsBiggerSide) {
          //Both closest points on the bigger side
          val c1: Byte = (aPoint & bPoint).toByte
          val c2: Byte = (aPoint | bPoint).toByte
          //Two contributions are permutations of (0,0,0,1) and (0,0,0,2) based on c1
          xsv_ext0 = xsb
          xsv_ext1 = xsb
          ysv_ext0 = ysb
          ysv_ext1 = ysb
          zsv_ext0 = zsb
          zsv_ext1 = zsb
          wsv_ext0 = wsb
          wsv_ext1 = wsb
          dx_ext0 = dx0 - SimplexNoise.SQUISH_CONSTANT_4D
          dy_ext0 = dy0 - SimplexNoise.SQUISH_CONSTANT_4D
          dz_ext0 = dz0 - SimplexNoise.SQUISH_CONSTANT_4D
          dw_ext0 = dw0 - SimplexNoise.SQUISH_CONSTANT_4D
          dx_ext1 = dx0 - 2 * SimplexNoise.SQUISH_CONSTANT_4D
          dy_ext1 = dy0 - 2 * SimplexNoise.SQUISH_CONSTANT_4D
          dz_ext1 = dz0 - 2 * SimplexNoise.SQUISH_CONSTANT_4D
          dw_ext1 = dw0 - 2 * SimplexNoise.SQUISH_CONSTANT_4D
          if ((c1 & 0x01) != 0) {
            xsv_ext0 += 1
            dx_ext0 -= 1
            xsv_ext1 += 2
            dx_ext1 -= 2
          }
          else if ((c1 & 0x02) != 0) {
            ysv_ext0 += 1
            dy_ext0 -= 1
            ysv_ext1 += 2
            dy_ext1 -= 2
          }
          else if ((c1 & 0x04) != 0) {
            zsv_ext0 += 1
            dz_ext0 -= 1
            zsv_ext1 += 2
            dz_ext1 -= 2
          }
          else {
            wsv_ext0 += 1
            dw_ext0 -= 1
            wsv_ext1 += 2
            dw_ext1 -= 2
          }
          //One contribution is a permutation of (1,1,1,-1) based on c2
          xsv_ext2 = xsb + 1
          ysv_ext2 = ysb + 1
          zsv_ext2 = zsb + 1
          wsv_ext2 = wsb + 1
          dx_ext2 = dx0 - 1 - 2 * SimplexNoise.SQUISH_CONSTANT_4D
          dy_ext2 = dy0 - 1 - 2 * SimplexNoise.SQUISH_CONSTANT_4D
          dz_ext2 = dz0 - 1 - 2 * SimplexNoise.SQUISH_CONSTANT_4D
          dw_ext2 = dw0 - 1 - 2 * SimplexNoise.SQUISH_CONSTANT_4D
          if ((c2 & 0x01) == 0) {
            xsv_ext2 -= 2
            dx_ext2 += 2
          }
          else if ((c2 & 0x02) == 0) {
            ysv_ext2 -= 2
            dy_ext2 += 2
          }
          else if ((c2 & 0x04) == 0) {
            zsv_ext2 -= 2
            dz_ext2 += 2
          }
          else {
            wsv_ext2 -= 2
            dw_ext2 += 2
          }
        }
        else {
          //Both closest points on the smaller side
          //One of the two extra points is (1,1,1,1)
          xsv_ext2 = xsb + 1
          ysv_ext2 = ysb + 1
          zsv_ext2 = zsb + 1
          wsv_ext2 = wsb + 1
          dx_ext2 = dx0 - 1 - 4 * SimplexNoise.SQUISH_CONSTANT_4D
          dy_ext2 = dy0 - 1 - 4 * SimplexNoise.SQUISH_CONSTANT_4D
          dz_ext2 = dz0 - 1 - 4 * SimplexNoise.SQUISH_CONSTANT_4D
          dw_ext2 = dw0 - 1 - 4 * SimplexNoise.SQUISH_CONSTANT_4D
          //Other two points are based on the shared axes.
          val c: Byte = (aPoint & bPoint).toByte
          if ((c & 0x01) != 0) {
            xsv_ext0 = xsb + 2
            xsv_ext1 = xsb + 1
            dx_ext0 = dx0 - 2 - 3 * SimplexNoise.SQUISH_CONSTANT_4D
            dx_ext1 = dx0 - 1 - 3 * SimplexNoise.SQUISH_CONSTANT_4D
          }
          else {
            xsv_ext1 = xsb
            xsv_ext0 = xsv_ext1
            dx_ext1 = dx0 - 3 * SimplexNoise.SQUISH_CONSTANT_4D
            dx_ext0 = dx_ext1
          }
          if ((c & 0x02) != 0) {
            ysv_ext1 = ysb + 1
            ysv_ext0 = ysv_ext1
            dy_ext1 = dy0 - 1 - 3 * SimplexNoise.SQUISH_CONSTANT_4D
            dy_ext0 = dy_ext1
            if ((c & 0x01) == 0) {
              ysv_ext0 += 1
              dy_ext0 -= 1
            }
            else {
              ysv_ext1 += 1
              dy_ext1 -= 1
            }
          }
          else {
            ysv_ext1 = ysb
            ysv_ext0 = ysb
            dy_ext1 = dy0 - 3 * SimplexNoise.SQUISH_CONSTANT_4D
            dy_ext0 = dy_ext1
          }
          if ((c & 0x04) != 0) {
            zsv_ext1 = zsb + 1
            zsv_ext0 = zsv_ext1
            dz_ext1 = dz0 - 1 - 3 * SimplexNoise.SQUISH_CONSTANT_4D
            dz_ext0 = dz_ext1
            if ((c & 0x03) == 0) {
              zsv_ext0 += 1
              dz_ext0 -= 1
            }
            else {
              zsv_ext1 += 1
              dz_ext1 -= 1
            }
          }
          else {
            zsv_ext1 = zsb
            zsv_ext0 = zsv_ext1
            dz_ext1 = dz0 - 3 * SimplexNoise.SQUISH_CONSTANT_4D
            dz_ext0 = dz_ext1
          }
          if ((c & 0x08) != 0) {
            wsv_ext0 = wsb + 1
            wsv_ext1 = wsb + 2
            dw_ext0 = dw0 - 1 - 3 * SimplexNoise.SQUISH_CONSTANT_4D
            dw_ext1 = dw0 - 2 - 3 * SimplexNoise.SQUISH_CONSTANT_4D
          }
          else {
            wsv_ext1 = wsb
            wsv_ext0 = wsv_ext1
            dw_ext1 = dw0 - 3 * SimplexNoise.SQUISH_CONSTANT_4D
            dw_ext0 = dw_ext1
          }
        }
      }
      else {
        //One point on each "side"
        var c1: Byte = 0
        var c2: Byte = 0
        if (aIsBiggerSide) {
          c1 = aPoint
          c2 = bPoint
        }
        else {
          c1 = bPoint
          c2 = aPoint
        }
        //Two contributions are the bigger-sided point with each 1 replaced with 2.
        if ((c1 & 0x01) != 0) {
          xsv_ext0 = xsb + 2
          xsv_ext1 = xsb + 1
          dx_ext0 = dx0 - 2 - 3 * SimplexNoise.SQUISH_CONSTANT_4D
          dx_ext1 = dx0 - 1 - 3 * SimplexNoise.SQUISH_CONSTANT_4D
        }
        else {
          xsv_ext1 = xsb
          xsv_ext0 = xsv_ext1
          dx_ext1 = dx0 - 3 * SimplexNoise.SQUISH_CONSTANT_4D
          dx_ext0 = dx_ext1
        }
        if ((c1 & 0x02) != 0) {
          ysv_ext1 = ysb + 1
          ysv_ext0 = ysv_ext1
          dy_ext1 = dy0 - 1 - 3 * SimplexNoise.SQUISH_CONSTANT_4D
          dy_ext0 = dy_ext1
          if ((c1 & 0x01) == 0) {
            ysv_ext0 += 1
            dy_ext0 -= 1
          }
          else {
            ysv_ext1 += 1
            dy_ext1 -= 1
          }
        }
        else {
          ysv_ext1 = ysb
          ysv_ext0 = ysv_ext1
          dy_ext1 = dy0 - 3 * SimplexNoise.SQUISH_CONSTANT_4D
          dy_ext0 = dy_ext1
        }
        if ((c1 & 0x04) != 0) {
          zsv_ext1 = zsb + 1
          zsv_ext0 = zsv_ext1
          dz_ext1 = dz0 - 1 - 3 * SimplexNoise.SQUISH_CONSTANT_4D
          dz_ext0 = dz_ext1
          if ((c1 & 0x03) == 0) {
            zsv_ext0 += 1
            dz_ext0 -= 1
          }
          else {
            zsv_ext1 += 1
            dz_ext1 -= 1
          }
        }
        else {
          zsv_ext1 = zsb
          zsv_ext0 = zsv_ext1
          dz_ext1 = dz0 - 3 * SimplexNoise.SQUISH_CONSTANT_4D
          dz_ext0 = dz_ext1
        }
        if ((c1 & 0x08) != 0) {
          wsv_ext0 = wsb + 1
          wsv_ext1 = wsb + 2
          dw_ext0 = dw0 - 1 - 3 * SimplexNoise.SQUISH_CONSTANT_4D
          dw_ext1 = dw0 - 2 - 3 * SimplexNoise.SQUISH_CONSTANT_4D
        }
        else {
          wsv_ext1 = wsb
          wsv_ext0 = wsv_ext1
          dw_ext1 = dw0 - 3 * SimplexNoise.SQUISH_CONSTANT_4D
          dw_ext0 = dw_ext1
        }
        //One contribution is a permutation of (1,1,1,-1) based on the smaller-sided point
        xsv_ext2 = xsb + 1
        ysv_ext2 = ysb + 1
        zsv_ext2 = zsb + 1
        wsv_ext2 = wsb + 1
        dx_ext2 = dx0 - 1 - 2 * SimplexNoise.SQUISH_CONSTANT_4D
        dy_ext2 = dy0 - 1 - 2 * SimplexNoise.SQUISH_CONSTANT_4D
        dz_ext2 = dz0 - 1 - 2 * SimplexNoise.SQUISH_CONSTANT_4D
        dw_ext2 = dw0 - 1 - 2 * SimplexNoise.SQUISH_CONSTANT_4D
        if ((c2 & 0x01) == 0) {
          xsv_ext2 -= 2
          dx_ext2 += 2
        }
        else if ((c2 & 0x02) == 0) {
          ysv_ext2 -= 2
          dy_ext2 += 2
        }
        else if ((c2 & 0x04) == 0) {
          zsv_ext2 -= 2
          dz_ext2 += 2
        }
        else {
          wsv_ext2 -= 2
          dw_ext2 += 2
        }
      }
      //Contribution (1,1,1,0)
      val dx4: Double = dx0 - 1 - 3 * SimplexNoise.SQUISH_CONSTANT_4D
      val dy4: Double = dy0 - 1 - 3 * SimplexNoise.SQUISH_CONSTANT_4D
      val dz4: Double = dz0 - 1 - 3 * SimplexNoise.SQUISH_CONSTANT_4D
      val dw4: Double = dw0 - 3 * SimplexNoise.SQUISH_CONSTANT_4D
      var attn4: Double = 2 - dx4 * dx4 - dy4 * dy4 - dz4 * dz4 - dw4 * dw4
      if (attn4 > 0) {
        attn4 *= attn4
        value += attn4 * attn4 * extrapolate(xsb + 1, ysb + 1, zsb + 1, wsb + 0, dx4, dy4, dz4, dw4)
      }
      //Contribution (1,1,0,1)
      val dx3: Double = dx4
      val dy3: Double = dy4
      val dz3: Double = dz0 - 3 * SimplexNoise.SQUISH_CONSTANT_4D
      val dw3: Double = dw0 - 1 - 3 * SimplexNoise.SQUISH_CONSTANT_4D
      var attn3: Double = 2 - dx3 * dx3 - dy3 * dy3 - dz3 * dz3 - dw3 * dw3
      if (attn3 > 0) {
        attn3 *= attn3
        value += attn3 * attn3 * extrapolate(xsb + 1, ysb + 1, zsb + 0, wsb + 1, dx3, dy3, dz3, dw3)
      }
      //Contribution (1,0,1,1)
      val dx2: Double = dx4
      val dy2: Double = dy0 - 3 * SimplexNoise.SQUISH_CONSTANT_4D
      val dz2: Double = dz4
      val dw2: Double = dw3
      var attn2: Double = 2 - dx2 * dx2 - dy2 * dy2 - dz2 * dz2 - dw2 * dw2
      if (attn2 > 0) {
        attn2 *= attn2
        value += attn2 * attn2 * extrapolate(xsb + 1, ysb + 0, zsb + 1, wsb + 1, dx2, dy2, dz2, dw2)
      }
      //Contribution (0,1,1,1)
      val dx1: Double = dx0 - 3 * SimplexNoise.SQUISH_CONSTANT_4D
      val dz1: Double = dz4
      val dy1: Double = dy4
      val dw1: Double = dw3
      var attn1: Double = 2 - dx1 * dx1 - dy1 * dy1 - dz1 * dz1 - dw1 * dw1
      if (attn1 > 0) {
        attn1 *= attn1
        value += attn1 * attn1 * extrapolate(xsb + 0, ysb + 1, zsb + 1, wsb + 1, dx1, dy1, dz1, dw1)
      }
      //Contribution (1,1,0,0)
      val dx5: Double = dx0 - 1 - 2 * SimplexNoise.SQUISH_CONSTANT_4D
      val dy5: Double = dy0 - 1 - 2 * SimplexNoise.SQUISH_CONSTANT_4D
      val dz5: Double = dz0 - 0 - 2 * SimplexNoise.SQUISH_CONSTANT_4D
      val dw5: Double = dw0 - 0 - 2 * SimplexNoise.SQUISH_CONSTANT_4D
      var attn5: Double = 2 - dx5 * dx5 - dy5 * dy5 - dz5 * dz5 - dw5 * dw5
      if (attn5 > 0) {
        attn5 *= attn5
        value += attn5 * attn5 * extrapolate(xsb + 1, ysb + 1, zsb + 0, wsb + 0, dx5, dy5, dz5, dw5)
      }
      //Contribution (1,0,1,0)
      val dx6: Double = dx0 - 1 - 2 * SimplexNoise.SQUISH_CONSTANT_4D
      val dy6: Double = dy0 - 0 - 2 * SimplexNoise.SQUISH_CONSTANT_4D
      val dz6: Double = dz0 - 1 - 2 * SimplexNoise.SQUISH_CONSTANT_4D
      val dw6: Double = dw0 - 0 - 2 * SimplexNoise.SQUISH_CONSTANT_4D
      var attn6: Double = 2 - dx6 * dx6 - dy6 * dy6 - dz6 * dz6 - dw6 * dw6
      if (attn6 > 0) {
        attn6 *= attn6
        value += attn6 * attn6 * extrapolate(xsb + 1, ysb + 0, zsb + 1, wsb + 0, dx6, dy6, dz6, dw6)
      }
      //Contribution (1,0,0,1)
      val dx7: Double = dx0 - 1 - 2 * SimplexNoise.SQUISH_CONSTANT_4D
      val dy7: Double = dy0 - 0 - 2 * SimplexNoise.SQUISH_CONSTANT_4D
      val dz7: Double = dz0 - 0 - 2 * SimplexNoise.SQUISH_CONSTANT_4D
      val dw7: Double = dw0 - 1 - 2 * SimplexNoise.SQUISH_CONSTANT_4D
      var attn7: Double = 2 - dx7 * dx7 - dy7 * dy7 - dz7 * dz7 - dw7 * dw7
      if (attn7 > 0) {
        attn7 *= attn7
        value += attn7 * attn7 * extrapolate(xsb + 1, ysb + 0, zsb + 0, wsb + 1, dx7, dy7, dz7, dw7)
      }
      //Contribution (0,1,1,0)
      val dx8: Double = dx0 - 0 - 2 * SimplexNoise.SQUISH_CONSTANT_4D
      val dy8: Double = dy0 - 1 - 2 * SimplexNoise.SQUISH_CONSTANT_4D
      val dz8: Double = dz0 - 1 - 2 * SimplexNoise.SQUISH_CONSTANT_4D
      val dw8: Double = dw0 - 0 - 2 * SimplexNoise.SQUISH_CONSTANT_4D
      var attn8: Double = 2 - dx8 * dx8 - dy8 * dy8 - dz8 * dz8 - dw8 * dw8
      if (attn8 > 0) {
        attn8 *= attn8
        value += attn8 * attn8 * extrapolate(xsb + 0, ysb + 1, zsb + 1, wsb + 0, dx8, dy8, dz8, dw8)
      }
      //Contribution (0,1,0,1)
      val dx9: Double = dx0 - 0 - 2 * SimplexNoise.SQUISH_CONSTANT_4D
      val dy9: Double = dy0 - 1 - 2 * SimplexNoise.SQUISH_CONSTANT_4D
      val dz9: Double = dz0 - 0 - 2 * SimplexNoise.SQUISH_CONSTANT_4D
      val dw9: Double = dw0 - 1 - 2 * SimplexNoise.SQUISH_CONSTANT_4D
      var attn9: Double = 2 - dx9 * dx9 - dy9 * dy9 - dz9 * dz9 - dw9 * dw9
      if (attn9 > 0) {
        attn9 *= attn9
        value += attn9 * attn9 * extrapolate(xsb + 0, ysb + 1, zsb + 0, wsb + 1, dx9, dy9, dz9, dw9)
      }
      //Contribution (0,0,1,1)
      val dx10: Double = dx0 - 0 - 2 * SimplexNoise.SQUISH_CONSTANT_4D
      val dy10: Double = dy0 - 0 - 2 * SimplexNoise.SQUISH_CONSTANT_4D
      val dz10: Double = dz0 - 1 - 2 * SimplexNoise.SQUISH_CONSTANT_4D
      val dw10: Double = dw0 - 1 - 2 * SimplexNoise.SQUISH_CONSTANT_4D
      var attn10: Double = 2 - dx10 * dx10 - dy10 * dy10 - dz10 * dz10 - dw10 * dw10
      if (attn10 > 0) {
        attn10 *= attn10
        value += attn10 * attn10 * extrapolate(xsb + 0, ysb + 0, zsb + 1, wsb + 1, dx10, dy10, dz10, dw10)
      }
    }
    //First extra vertex
    var attn_ext0: Double = 2 - dx_ext0 * dx_ext0 - dy_ext0 * dy_ext0 - dz_ext0 * dz_ext0 - dw_ext0 * dw_ext0
    if (attn_ext0 > 0) {
      attn_ext0 *= attn_ext0
      value += attn_ext0 * attn_ext0 * extrapolate(xsv_ext0, ysv_ext0, zsv_ext0, wsv_ext0, dx_ext0, dy_ext0, dz_ext0, dw_ext0)
    }
    //Second extra vertex
    var attn_ext1: Double = 2 - dx_ext1 * dx_ext1 - dy_ext1 * dy_ext1 - dz_ext1 * dz_ext1 - dw_ext1 * dw_ext1
    if (attn_ext1 > 0) {
      attn_ext1 *= attn_ext1
      value += attn_ext1 * attn_ext1 * extrapolate(xsv_ext1, ysv_ext1, zsv_ext1, wsv_ext1, dx_ext1, dy_ext1, dz_ext1, dw_ext1)
    }
    //Third extra vertex
    var attn_ext2: Double = 2 - dx_ext2 * dx_ext2 - dy_ext2 * dy_ext2 - dz_ext2 * dz_ext2 - dw_ext2 * dw_ext2
    if (attn_ext2 > 0) {
      attn_ext2 *= attn_ext2
      value += attn_ext2 * attn_ext2 * extrapolate(xsv_ext2, ysv_ext2, zsv_ext2, wsv_ext2, dx_ext2, dy_ext2, dz_ext2, dw_ext2)
    }
    value / SimplexNoise.NORM_CONSTANT_4D
  }

  private def extrapolate(xsb: Int, ysb: Int, dx: Double, dy: Double): Double = {
    val index: Int = perm((perm(xsb & 0xFF) + ysb) & 0xFF) & 0x0E
    SimplexNoise.gradients2D(index) * dx + SimplexNoise.gradients2D(index + 1) * dy
  }

  private def extrapolate(xsb: Int, ysb: Int, zsb: Int, dx: Double, dy: Double, dz: Double): Double = {
    val index: Int = permGradIndex3D((perm((perm(xsb & 0xFF) + ysb) & 0xFF) + zsb) & 0xFF)
    SimplexNoise.gradients3D(index) * dx + SimplexNoise.gradients3D(index + 1) * dy + SimplexNoise.gradients3D(index + 2) * dz
  }

  private def extrapolate(xsb: Int, ysb: Int, zsb: Int, wsb: Int, dx: Double, dy: Double, dz: Double, dw: Double): Double = {
    val index: Int = perm((perm((perm((perm(xsb & 0xFF) + ysb) & 0xFF) + zsb) & 0xFF) + wsb) & 0xFF) & 0xFC
    SimplexNoise.gradients4D(index) * dx + SimplexNoise.gradients4D(index + 1) * dy + SimplexNoise.gradients4D(index + 2) * dz + SimplexNoise.gradients4D(index + 3) * dw
  }
}
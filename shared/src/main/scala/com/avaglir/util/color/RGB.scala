package com.avaglir.util.color

import com.avaglir.util.numeric.Imports._

case class RGB(red: Int, green: Int, blue: Int) extends Color {
  override lazy val (hue, saturation, luminance) = {
    val r = (red.toFloat / 255).clamp
    val g = (green.toFloat / 255).clamp
    val b = (blue.toFloat / 255).clamp

    val max = maxOf(r, g, b)
    val min = minOf(r, g, b)

    val lum = (max + min) / 2

    if (max == min) {
      (0f.unitClamped, 0f.unitClamped, lum.unitClamped)
    } else {
      val d = max - min
      val sat = if (lum > 0.5) d / (2 - max - min) else d / (max + min)

      val hue = (max match {
        case x if x == r => (g - b) / d + (if (g < b) 6 else 0)
        case x if x == g => (b - r) / d + 2
        case x if x == b => (r - g) / d + 4
      })/6

      (hue.unitClamped, sat.unitClamped, lum.unitClamped)
    }
  }
}

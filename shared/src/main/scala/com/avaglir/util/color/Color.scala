package com.avaglir.util.color

import com.avaglir.util.numeric.UnitClampedFloat

sealed abstract class Color {
  def red: Int
  def green: Int
  def blue: Int

  def hue: UnitClampedFloat
  def saturation: UnitClampedFloat
  def luminance: UnitClampedFloat

  lazy val hex: String = f"#$red%02x$green%02x$blue%02x"

  def hsl = HSL(hue, saturation, luminance)
  def rgb = RGB(red, green, blue)

  def darker = HSL(hue, saturation, UnitClampedFloat(luminance.value/2))
  def lighter = HSL(hue, saturation, UnitClampedFloat((luminance.value + 1f)/2))

  def desaturated = HSL(hue, UnitClampedFloat(saturation.value/2), luminance)
  def saturated = HSL(hue, UnitClampedFloat((saturation.value + 1f)/2), luminance)
}

object Color {
  val WHITE = RGB(0xff, 0xff, 0xff)
  val BLACK = RGB(0, 0, 0)
  val RED = RGB(0xff, 0, 0)
  val GREEN = RGB(0, 0xff, 0)
  val BLUE = RGB(0, 0, 0xff)
  val YELLOW = RGB(0xff, 0xff, 0)
  val CYAN = RGB(0, 0xff, 0xff)
  val MAGENTA = RGB(0xff, 0, 0xff)

  private val matchRegex = "#([0-9a-f]{2})([0-9a-f]{2})([0-9a-f]{2})".r

  def apply(s: String): RGB = {
    val matchRegex(r, g, b) = s
    val (red :: green :: blue :: _) = List(r, g, b).map(Integer.parseInt(_, 16) % 255)
    RGB(red, green, blue)
  }
}

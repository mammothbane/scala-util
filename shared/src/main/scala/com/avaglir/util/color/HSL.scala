package com.avaglir.util.color

case class HSL(hue: UnitClampedFloat, saturation: UnitClampedFloat, luminance: UnitClampedFloat) extends Color {
  lazy val (red, green, blue) = {
    val lumT = (luminance * 255).toInt

    if (saturation.value == 0) (lumT, lumT, lumT) else {
      val q = if (luminance < 0.5) luminance * (1 + saturation) else (luminance + saturation) - luminance * saturation
      val p = 2 * luminance - q

      def hue2rgb(t: Float): Int = {
        val t2 = if (t < 0) t + 1
          else if (t > 1) t - 1
          else t

        val out =
          if (t2 < 1f/6) p + (q - p) * 6 * t2
          else if (t2 < 0.5) q
          else if (t2 < 2f/3) p + (q - p) * (2f/3 - t2) * 6
          else p

        (out * 255).toInt
      }

      (hue2rgb(hue + (1f/3)), hue2rgb(hue), hue2rgb(hue - (1f/3)))
    }
  }
}

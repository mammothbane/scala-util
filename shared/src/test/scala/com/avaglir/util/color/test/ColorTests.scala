package com.avaglir.util.color.test

import com.avaglir.util.color.{HSL, RGB}
import com.avaglir.util.numeric._
import utest._
import utest.framework.{Test, Tree}

object ColorTests extends TestSuite {
  val tests: Tree[Test] = this {
    'RGB_HSL_match {
      val pairs = List(
        ((136f, 0.9f, 0.61f), (66, 245, 113)),
        ((71f, 0.69f, 0.28f), (102, 120, 22))
      )

      pairs.zipWithIndex.foreach { case ((hsl, rgb), idx) =>
        val (h, s, l) = hsl
        val (r, g, b) = rgb

        val hslColor = HSL((h/360f).unitClamped, s, l)
        val rgbColor = RGB(r, g, b)

        hslColor.red    ==> rgbColor.red
        hslColor.green  ==> rgbColor.green
        hslColor.blue   ==> rgbColor.blue

        assert(math.abs(hslColor.hue        - rgbColor.hue)         < 0.1)
        assert(math.abs(hslColor.saturation - rgbColor.saturation)  < 0.1)
        assert(math.abs(hslColor.luminance  - rgbColor.luminance)   < 0.1)
      }
    }
  }
}

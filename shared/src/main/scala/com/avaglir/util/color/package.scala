package com.avaglir.util

package object color {
  private final val colorRegex = "%c{#[0-9a-f]{6}}%b{#[0-9a-f]{6}}".r
  implicit class strExt(s: String) {
    def colorize(fg: Color = Color.WHITE, bg: Color = Color.BLACK): String = s"%c{${fg.hex}}%b{${bg.hex}}$s"
    def decolorize: String = colorRegex.replaceAllIn(s, "")
  }
}

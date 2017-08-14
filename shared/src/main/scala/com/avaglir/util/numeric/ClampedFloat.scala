package com.avaglir.util.numeric

class ClampedFloat(v: Float, min: Float = 0, max: Float = 1) extends Clamped[Float](v, min, max)

final case class UnitClampedFloat(v: Float) extends ClampedFloat(v)

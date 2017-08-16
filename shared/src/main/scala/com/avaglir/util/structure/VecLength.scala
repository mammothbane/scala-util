package com.avaglir.util.structure

sealed abstract class VecLength(v: Int)

object VecLength {
  case object VL0 extends VecLength(0)
  case object VL1 extends VecLength(1)
  case object VL2 extends VecLength(2)
  case object VL3 extends VecLength(3)
  case object VL4 extends VecLength(4)
  case object VL5 extends VecLength(5)
  case object VL6 extends VecLength(6)
  case object VL7 extends VecLength(7)
  case object VL8 extends VecLength(8)
  case object VL9 extends VecLength(9)
}

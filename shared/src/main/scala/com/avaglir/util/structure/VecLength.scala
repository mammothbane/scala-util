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

  sealed trait Length[T <: VecLength] {
    def length: Int
  }

  object Lengths {
    implicit object L0 extends Length[VL0.type] { def length = 0 }
    implicit object L1 extends Length[VL1.type] { def length = 1 }
    implicit object L2 extends Length[VL2.type] { def length = 2 }
    implicit object L3 extends Length[VL3.type] { def length = 3 }
    implicit object L4 extends Length[VL4.type] { def length = 4 }
    implicit object L5 extends Length[VL5.type] { def length = 5 }
    implicit object L6 extends Length[VL6.type] { def length = 6 }
    implicit object L7 extends Length[VL7.type] { def length = 7 }
    implicit object L8 extends Length[VL8.type] { def length = 8 }
    implicit object L9 extends Length[VL9.type] { def length = 9 }
  }
}

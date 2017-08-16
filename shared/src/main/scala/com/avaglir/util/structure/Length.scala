package com.avaglir.util.structure

import com.avaglir.util.structure.VecLength._

sealed trait Length[T <: VecLength] {
  def length: Int
}
object Length {
  implicit case object L0 extends Length[VL0.type] { def length = 0 }
  implicit case object L1 extends Length[VL1.type] { def length = 1 }
  implicit case object L2 extends Length[VL2.type] { def length = 2 }
  implicit case object L3 extends Length[VL3.type] { def length = 3 }
  implicit case object L4 extends Length[VL4.type] { def length = 4 }
  implicit case object L5 extends Length[VL5.type] { def length = 5 }
  implicit case object L6 extends Length[VL6.type] { def length = 6 }
  implicit case object L7 extends Length[VL7.type] { def length = 7 }
  implicit case object L8 extends Length[VL8.type] { def length = 8 }
  implicit case object L9 extends Length[VL9.type] { def length = 9 }
}

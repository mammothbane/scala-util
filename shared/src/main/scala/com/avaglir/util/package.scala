package com.avaglir

import com.avaglir.util.structure.Vector2

package object util {
  type IntVec = Vector2[Int]

  sealed abstract class TypeLength(length: Int)
  object TypeLength {
    case object Zero extends TypeLength(0)
    case object One extends TypeLength(1)
    case object Two extends TypeLength(2)
    case object Three extends TypeLength(3)
    case object Four extends TypeLength(4)
    case object Five extends TypeLength(5)
    case object Six extends TypeLength(6)
    case object Seven extends TypeLength(7)
    case object Eight extends TypeLength(8)
    case object Nine extends TypeLength(9)
    case object Ten extends TypeLength(10)
  }

  sealed trait LengthLookup[_ <: TypeLength] {
    def length: Int
  }

  implicit case object L0 extends LengthLookup[TypeLength.Zero.type] { def length = 0 }
  implicit case object L1 extends LengthLookup[TypeLength.One.type] { def length = 1 }
  implicit case object L2 extends LengthLookup[TypeLength.Two.type] { def length = 2 }
  implicit case object L3 extends LengthLookup[TypeLength.Three.type] { def length = 3 }
  implicit case object L4 extends LengthLookup[TypeLength.Four.type] { def length = 4 }
  implicit case object L5 extends LengthLookup[TypeLength.Five.type] { def length = 5 }
  implicit case object L6 extends LengthLookup[TypeLength.Six.type] { def length = 6 }
  implicit case object L7 extends LengthLookup[TypeLength.Seven.type] { def length = 7 }
  implicit case object L8 extends LengthLookup[TypeLength.Eight.type] { def length = 8 }
  implicit case object L9 extends LengthLookup[TypeLength.Nine.type] { def length = 9 }
  implicit case object L10 extends LengthLookup[TypeLength.Nine.type] { def length = 10 }
}

package com.avaglir.util.geometry

import com.avaglir.util.structure._

import scala.reflect.ClassTag

trait Hull {
  def apply[U : Numeric : ClassTag, L <: VecLength : Length](p: Vector[U, L]*): Seq[Vector[U, L]]
}

object Hull {

}

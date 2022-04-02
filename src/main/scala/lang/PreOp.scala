package com.github.jtrim777.cmm
package lang

sealed abstract class PreOp(val sym: String) {
  def toCode: String = sym
}

object PreOp {
  case object BitFlip extends PreOp("~")
}



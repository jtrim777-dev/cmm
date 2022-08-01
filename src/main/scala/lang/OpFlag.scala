package dev.jtrim777.cmm
package lang

sealed abstract class OpFlag(val sym: String) {
  def toCode: String = sym
}

object OpFlag {
  case object Unsigned extends OpFlag("u")
  case object Floating extends OpFlag("f")
  case object Trapping extends OpFlag("t")
}

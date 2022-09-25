package dev.jtrim777.cmm
package isa.x86_64

sealed abstract class OpdSize(val flag: String) extends InstrSuffix {
  override def key: String = flag

  def +(other: OpdSize): OpdSize = OpdSize.Pair(this, other)
}

object OpdSize {
  case object Byt extends OpdSize("b")

  case object Word extends OpdSize("w")

  case object DblWord extends OpdSize("l")

  case object QuadWord extends OpdSize("q")

  case class Pair(a: OpdSize, b: OpdSize) extends OpdSize(a.flag + b.flag)
}

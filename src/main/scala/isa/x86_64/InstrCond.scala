package dev.jtrim777.cmm
package isa.x86_64

sealed abstract class InstrCond(val flag: String) extends InstrSuffix {
  override def key: String = flag
}

object InstrCond {
  case object Eq extends InstrCond("e")

  case object NEq extends InstrCond("ne")

  case object Lt extends InstrCond("l")

  case object LEq extends InstrCond("le")

  case object Gt extends InstrCond("g")

  case object GEq extends InstrCond("ge")

  case object Zero extends InstrCond("z")

  case object NZero extends InstrCond("nz")

  case object Above extends InstrCond("a")

  case object NAbove extends InstrCond("na")

  case object Below extends InstrCond("b")

  case object NBelow extends InstrCond("nb")
}

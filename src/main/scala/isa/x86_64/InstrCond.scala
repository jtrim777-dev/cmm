package dev.jtrim777.cmm
package isa.x86_64

sealed abstract class InstrCond(val flag: String, opposite: InstrCond) extends InstrSuffix {
  override def key: String = flag
  def negate: InstrCond = opposite
}

object InstrCond {
  case object Eq extends InstrCond("e", NEq)

  case object NEq extends InstrCond("ne", Eq)

  case object Lt extends InstrCond("l", GEq)

  case object LEq extends InstrCond("le", Gt)

  case object Gt extends InstrCond("g", LEq)

  case object GEq extends InstrCond("ge", Lt)

  case object Zero extends InstrCond("z", NZero)

  case object NZero extends InstrCond("nz", Zero)

  case object Above extends InstrCond("a", NAbove)

  case object NAbove extends InstrCond("na", Above)

  case object Below extends InstrCond("b", NBelow)

  case object NBelow extends InstrCond("nb", Below)
}

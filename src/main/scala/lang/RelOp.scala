package dev.jtrim777.cmm.lang

sealed abstract class RelOp(val sym: String) {
  val flags: Seq[OpFlag]

  def toCode: String = sym + flags.map(_.toCode).mkString

  def negate: RelOp = this match {
    case RelOp.EQ(flags@_*) => RelOp.NE(flags)
    case RelOp.NE(flags@_*) => RelOp.EQ(flags)
    case RelOp.LT(flags@_*) => RelOp.GE(flags)
    case RelOp.LE(flags@_*) => RelOp.GT(flags)
    case RelOp.GT(flags@_*) => RelOp.LE(flags)
    case RelOp.GE(flags@_*) => RelOp.LT(flags)
  }

  def unsigned: Boolean = flags.contains(OpFlag.Unsigned)
  def floating: Boolean = flags.contains(OpFlag.Floating)
}

object RelOp {
  case class EQ(flags: OpFlag*) extends RelOp("==")
  case class NE(flags: OpFlag*) extends RelOp("!=")

  case class LT(flags: OpFlag*) extends RelOp("<")
  case class LE(flags: OpFlag*) extends RelOp("<=")

  case class GT(flags: OpFlag*) extends RelOp(">")
  case class GE(flags: OpFlag*) extends RelOp(">=")
}

package com.github.jtrim777.cmm
package lang

sealed abstract class ArithOp(val sym: String) {
  val flags: Seq[OpFlag]

  def toCode: String = sym + flags.map(_.sym).mkString
}

object ArithOp {
  case class Add(flags: OpFlag*) extends ArithOp("+")
  case class Sub(flags: OpFlag*) extends ArithOp("-")
  case class Mul(flags: OpFlag*) extends ArithOp("*")
  case class Div(flags: OpFlag*) extends ArithOp("/")
  case class Mod(flags: OpFlag*) extends ArithOp("%")

  case object And extends ArithOp("&") { val flags: Seq[OpFlag] = Seq.empty }
  case object Or extends ArithOp("|") { val flags: Seq[OpFlag] = Seq.empty }
  case object Xor extends ArithOp("^") { val flags: Seq[OpFlag] = Seq.empty }
  case object ShiftL extends ArithOp("<<") { val flags: Seq[OpFlag] = Seq.empty }
  case class ShiftR(signed: Boolean) extends ArithOp(">>") {
    override val flags: Seq[OpFlag] = if (signed) Seq.empty else Seq(OpFlag.Unsigned)
  }
}



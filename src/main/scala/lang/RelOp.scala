package com.github.jtrim777.cmm
package lang

sealed abstract class RelOp(val sym: String) {
  val flags: Seq[OpFlag]

  def toCode: String = sym + flags.map(_.toCode).mkString
}

object RelOp {
  case class EQ(flags: OpFlag*) extends RelOp("==")
  case class NE(flags: OpFlag*) extends RelOp("!=")

  case class LT(flags: OpFlag*) extends RelOp("<")
  case class LE(flags: OpFlag*) extends RelOp("<=")

  case class GT(flags: OpFlag*) extends RelOp(">")
  case class GE(flags: OpFlag*) extends RelOp(">=")
}

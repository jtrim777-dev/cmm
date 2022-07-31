package com.github.jtrim777.cmm
package lang

sealed abstract class ArithOp(val sym: String) {
  val flags: Seq[OpFlag]

  def toCode: String = sym + flags.map(_.sym).mkString

  def unsigned: Boolean = flags.contains(OpFlag.Unsigned)
  def floating: Boolean = flags.contains(OpFlag.Floating)
  def trapping: Boolean = flags.contains(OpFlag.Trapping)
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

  val SymMap: Map[String, List[OpFlag] => ArithOp] = Map(
    "+" -> {ops => Add(ops:_*)},
    "-" -> {ops => Sub(ops:_*)},
    "*" -> {ops => Mul(ops:_*)},
    "/" -> {ops => Div(ops:_*)},
    "%" -> {ops => Mod(ops:_*)},
    "&" -> {_ => And},
    "|" -> {_ => Or},
    "^" -> {_ => Xor},
    "<<" -> {_ => ShiftL},
    ">>" -> {flags => ShiftR(!flags.contains(OpFlag.Unsigned))},
  )

  def parse(str: String, flags: Seq[OpFlag]): ArithOp = {
    SymMap(str)(flags.toList)
  }
}



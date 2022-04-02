package com.github.jtrim777.cmm
package lang

sealed trait Primitive {
  val flags: Seq[OpFlag]

  def toCode: String = {
    getClass.getSimpleName.toLowerCase + flags.map(_.toCode).mkString
  }
}

object Primitive {
  case class WordCast(target: Int) extends Primitive {
    override val flags: Seq[OpFlag] = Seq.empty

    override def toCode: String = s"word$target"
  }
  case class FlotCast(target: Int) extends Primitive {
    override val flags: Seq[OpFlag] = Seq.empty

    override def toCode: String = s"float$target"
  }

  case class Neg(flags: OpFlag*) extends Primitive
  case class Abs(flags: OpFlag*) extends Primitive

  case class Sign(flags: OpFlag*) extends Primitive

  case class Trunc(flags: OpFlag*) extends Primitive
  case class Round(flags: OpFlag*) extends Primitive
}

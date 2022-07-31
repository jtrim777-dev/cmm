package com.github.jtrim777.cmm
package lang

sealed trait Primitive {
  val flags: Seq[OpFlag]

  def toCode: String = {
    getClass.getSimpleName.toLowerCase + flags.map(_.toCode).mkString
  }

  def output(inputs: Seq[DataType]): DataType

  def unsigned: Boolean = flags.contains(OpFlag.Unsigned)
  def floating: Boolean = flags.contains(OpFlag.Floating)
  def trapping: Boolean = flags.contains(OpFlag.Trapping)
}

object Primitive {
  case class WordCast(target: Int) extends Primitive {
    override val flags: Seq[OpFlag] = Seq.empty

    override def toCode: String = s"word$target"

    override def output(inputs: Seq[DataType]): DataType = target match {
      case 1 => DataType.Word1
      case 2 => DataType.Word2
      case 4 => DataType.Word4
      case 8 => DataType.Word8
    }
  }
  case class FlotCast(target: Int) extends Primitive {
    override val flags: Seq[OpFlag] = Seq.empty

    override def toCode: String = s"float$target"

    override def output(inputs: Seq[DataType]): DataType = target match {
      case 4 => DataType.Flot4
      case 8 => DataType.Flot8
    }
  }

  case class Neg(flags: OpFlag*) extends Primitive {
    override def output(inputs: Seq[DataType]): DataType = inputs.head
  }
  case class Abs(flags: OpFlag*) extends Primitive {
    override def output(inputs: Seq[DataType]): DataType = inputs.head
  }

  case class Sign(flags: OpFlag*) extends Primitive {
    override def output(inputs: Seq[DataType]): DataType = DataType.Word1
  }

  case class Trunc(flags: OpFlag*) extends Primitive {
    override def output(inputs: Seq[DataType]): DataType = inputs.head
  }

  case class Round(flags: OpFlag*) extends Primitive {
    override def output(inputs: Seq[DataType]): DataType = inputs.head
  }
}

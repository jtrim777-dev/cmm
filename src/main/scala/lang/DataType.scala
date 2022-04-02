package com.github.jtrim777.cmm
package lang

sealed trait DataType {
  val superName: String
  val bytes: Int

  def toCode: String = superName + bytes

  def canWidenTo(other: DataType): Boolean
}

object DataType {
  sealed abstract class Word(val bytes: Int) extends DataType {
    val superName: String = "word"

    override def canWidenTo(other: DataType): Boolean = other match {
      case w:Word => w.bytes >= this.bytes
      case _ => false
    }
  }
  case object Word1 extends Word(1)
  case object Word2 extends Word(2)
  case object Word4 extends Word(4)
  case object Word8 extends Word(8)

  sealed abstract class Flot(val bytes: Int) extends DataType {
    val superName: String = "float"

    override def canWidenTo(other: DataType): Boolean = other match {
      case f:Flot => f.bytes >= this.bytes
      case _ => false
    }
  }
  case object Flot4 extends Flot(4)
  case object Flot8 extends Flot(8)
}

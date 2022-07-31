package com.github.jtrim777.cmm
package lang

import java.lang.{Long => JLong}

sealed trait Expression {
  def toCode: String
  def toWrappedCode: String = if (needParen) s"($toCode)" else toCode
  def needParen: Boolean = false
}

object Expression {
  case class CInt(value: Long) extends Expression {
    override def toCode: String = value.toString
  }
  object CInt {
    def parse(raw: String): Long = {
      if (raw.contains("0x")) {
        JLong.parseLong(raw.replace("0x", ""), 16)
      } else if (raw.contains("0b")) {
        JLong.parseLong(raw.replace("0b", ""), 2)
      } else {
        raw.toLong
      }
    }
  }

  case class CFlot(value: Double) extends Expression {
    override def toCode: String = value.toString
  }

  case class ID(name: String) extends Expression {
    override def toCode: String = name
  }

  case class Read(kind: DataType, pos: Expression, align: Option[Long] = None) extends Expression {
    override def toCode: String = {
      val as = align.map(i => s"{align$i}").getOrElse("")

      s"${kind.toCode}$as[${pos.toCode}]"
    }
  }

  case class InfixOp(lhs: Expression, op: ArithOp, rhs: Expression) extends Expression {
    override def toCode: String = s"${lhs.toCode} ${op.toCode} ${rhs.toCode}"

    override def needParen: Boolean = true
  }
  case class PrefixOp(op: PreOp, target: Expression) extends Expression {
    override def toCode: String = s"${op.toCode}${target.toCode}"
  }
  case class PrimOp(op: Primitive, args: Seq[Expression]) extends Expression {
    override def toCode: String = s"${op.toCode}(${args.map(_.toCode).mkString(", ")})"
  }

}

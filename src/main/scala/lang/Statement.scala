package com.github.jtrim777.cmm
package lang

sealed trait Statement {
  def toCode: String
}

object Statement {
  case object Skip extends Statement {
    override def toCode: String = "skip;"
  }

  case class VarDecl(kind: DataType, names: String*) extends Statement {
    override def toCode: String = kind.toCode + " " + names.mkString(", ") + ";"
  }
  case class Assn(name: String, value: Expression) extends Statement {
    override def toCode: String = s"$name = ${value.toCode};"
  }

  case class Write(kind: DataType, pos: Expression, value: Expression,
                   align: Option[Int] = None) extends Statement {
    override def toCode: String = {
      val as = align.map(i => s"{align$i}").getOrElse("")

      s"${kind.toCode}$as[${pos.toCode}] = ${value.toCode};"
    }
  }

  case class IfStmt(lhs: Expression, op: RelOp, rhs: Expression,
                    exec: Block, elseExec: Option[Block]) extends Statement {
    override def toCode: String = {
      val ep = elseExec.map(b => s" else {\n${b.toCode}\n}").getOrElse("")

      s"if ${lhs.toWrappedCode} ${op.toCode} ${rhs.toWrappedCode} {\n${exec.toCode}\n}$ep\n"
    }
  }

  case class LocalLabel(name: String) extends Statement {
    override def toCode: String = s"$name: "
  }
  case class Goto(label: String) extends Statement {
    override def toCode: String = s"goto $label;"
  }

  case class Jump(proc: Expression, args: Expression*) extends Statement {
    override def toCode: String = if (args.nonEmpty) {
      s"jump ${proc.toCode}(${args.map(_.toCode).mkString(", ")});"
    } else {
      s"jump ${proc.toCode};"
    }
  }
  case class Call(results: Seq[String], proc: Expression, args: Expression*) extends Statement {
    override def toCode: String = if (results.isEmpty) {
      s"${proc.toCode}(${args.map(_.toCode).mkString(", ")});"
    } else {
      s"${results.mkString(", ")} = ${proc.toCode}(${args.map(_.toCode).mkString(", ")});"
    }
  }

  case class Return(results: Expression*) extends Statement {
    override def toCode: String = if (results.nonEmpty) {
      s"return (${results.map(_.toCode).mkString(", ")});"
    } else "return;"
  }

  case class Block(stmts: Statement*) extends Statement {
    override def toCode: String =
      stmts.map(_.toCode).flatMap(_.split('\n')).map(s => "  " + s).mkString("{\n", "\n", "\n}\n")

    def byteUsage: Int = {
      val subblocks = stmts.collect {
        case b:Block => b
      }

      val decls = stmts.collect {
        case d:VarDecl => d
      }

      val base = decls.map(d => d.names.length * d.kind.bytes).sum
      val greatest = subblocks.map(_.byteUsage).maxOption.getOrElse(0)

      base + greatest
    }
  }
}

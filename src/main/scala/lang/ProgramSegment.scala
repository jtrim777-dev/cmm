package dev.jtrim777.cmm
package lang

import collection.immutable.Seq

trait ProgramSegment {
  def toCode: String
}

object ProgramSegment {
  trait SymbolPort extends ProgramSegment
  case class SymbolImport(name: String) extends SymbolPort {
    override def toCode: String = s"import $name;"
  }
  case class SymbolExport(name: String) extends SymbolPort {
    override def toCode: String = s"export $name;"
  }

  case class DataBlock(decls: Seq[DataDecl]) extends ProgramSegment {
    override def toCode: String = {
      val declList: String = decls.map(_.toCode).map(s => "    "+s).mkString("\n")

      s"data {\n$declList\n}\n"
    }
  }

  case class ProcDefn(name: String, params: Seq[(String, DataType)],
                      body: Statement.Block) extends ProgramSegment {
    override def toCode: String = {
      val bodyText: String = body.toCode

      val paramList = params.map(t => s"${t._2.toCode} ${t._1}").mkString(", ")

      s"$name($paramList) $bodyText"
    }

    def byteUsage: Int = body.byteUsage

    def wordUsage(wordSize: Int): Int = {
      Math.ceil(byteUsage.toDouble / wordSize.toDouble).toInt
    }
  }
}

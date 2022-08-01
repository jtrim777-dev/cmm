package dev.jtrim777.cmm.lang

import dev.jtrim777.cmm.lang.ProgramSegment.{DataBlock, ProcDefn}

case class Program(segments: ProgramSegment*) {
  def toCode: String = {
    segments.map(_.toCode).mkString("\n")
  }

  def dataBlocks: Seq[DataBlock] = segments collect {
    case db:DataBlock => db
  }

  def procedures: Seq[ProcDefn] = segments collect {
    case db:ProcDefn => db
  }

  def ports: Seq[ProgramSegment] = segments collect {
    case is:ProgramSegment.SymbolImport => is
    case is:ProgramSegment.SymbolExport => is
  }
}

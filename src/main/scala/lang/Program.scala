package dev.jtrim777.cmm
package lang

import lang.ProgramSegment.{DataBlock, ProcDefn, SymbolPort}
import collection.immutable.Seq

case class Program(ports: Seq[SymbolPort], dataBlocks: Seq[DataBlock], procedures: Seq[ProcDefn]) {
  def toCode: String = {
    (ports ++ dataBlocks ++ procedures).map(_.toCode).mkString("\n")
  }

  def imports: Seq[ProgramSegment.SymbolImport] = ports.collect {
    case i:ProgramSegment.SymbolImport => i
  }

  def exports: Seq[ProgramSegment.SymbolExport] = ports.collect {
    case i: ProgramSegment.SymbolExport => i
  }

  def dataHead: DataBlock = if (dataBlocks.isEmpty) DataBlock(Seq.empty) else dataBlocks.head
}

object Program {
  def fromMix(segments: Seq[ProgramSegment]): Program = {
    val ports = segments.collect {
      case p: ProgramSegment.SymbolPort => p
    }

    val datas = segments.collect {
      case p: ProgramSegment.DataBlock => p
    }

    val procs = segments.collect {
      case p: ProgramSegment.ProcDefn => p
    }

    Program(ports, datas, procs)
  }
}

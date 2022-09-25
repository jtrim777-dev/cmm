package dev.jtrim777.cmm
package lang

import lang.ProgramSegment.{DataBlock, ProcDefn, SymbolPort}
import collection.immutable.Seq

case class Program(ports: Seq[SymbolPort], dataBlocks: Seq[DataBlock], procedures: Seq[ProcDefn]) {
  def toCode: String = {
    (ports ++ dataBlocks ++ procedures).map(_.toCode).mkString("\n")
  }
}

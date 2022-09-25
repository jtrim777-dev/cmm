package dev.jtrim777.cmm
package compile

import isa.{ISA, ISeq}
import lang.ProgramSegment.DataBlock

import collection.immutable.Seq

case class CompiledProgram[Arch <: ISA](data: DataBlock, execBlocks: Seq[ISeq[Arch]],
                                        importedSymbols: Seq[String], exportedSymbols: Seq[String])

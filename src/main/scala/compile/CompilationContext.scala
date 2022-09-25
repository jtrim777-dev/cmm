package dev.jtrim777.cmm
package compile

import compile.CompilationContext.ScopeObject
import isa.{ISA, ISArg, VirtualRegister}
import lang.DataType

case class CompilationContext[Arch <: ISA](scope: Map[String, ScopeObject[Arch]], frameDepth: Int, procArity: Int) {
  def enscopeLocal(name: String, kind: DataType, pos: VirtualRegister): CompilationContext[Arch] = {
    this.copy(scope = scope.updated(name, CompilationContext.LocalVar(kind, pos)))
  }
  def setArity(a: Int): CompilationContext[Arch] = this.copy(procArity = a)

  def addLocal(name: String, kind: DataType, wordSize: Int): CompilationContext[Arch] = {
    val pos = VirtualRegister.StackOffset(frameDepth)
    val nscope = scope.updated(name, CompilationContext.LocalVar(kind, pos))

    this.copy(scope = nscope, frameDepth = frameDepth + wordSize)
  }

  def enscope(name: String, value: CompilationContext.ScopeObject[Arch]): CompilationContext[Arch] =
    this.copy(scope = scope.updated(name, value))
}

object CompilationContext {
  sealed trait ScopeObject[Arch <: ISA]
  case class ProcParam[Arch <: ISA](name: String, index: Int, kind: DataType) extends ScopeObject[Arch]
  case class LocalVar[Arch <: ISA](kind: DataType, pos: VirtualRegister) extends ScopeObject[Arch]
  case class Procedure[Arch <: ISA](name: String) extends ScopeObject[Arch]
  case class DataLabel[Arch <: ISA](kind: DataType, size: Int, ref: ISArg.LabelRef) extends ScopeObject[Arch]
}

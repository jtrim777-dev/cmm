package dev.jtrim777.cmm
package common

import dev.jtrim777.cmm.asm.ISArg.LabelRef
import common.Context.ScopeObject
import lang.DataType

case class Context(scope: Map[String, ScopeObject], frameDepth: Int, procArity: Int) {
  def enscopeLocal(name: String, kind: DataType, pos: VirtualRegister): Context = {
    this.copy(scope = scope.updated(name, Context.LocalVar(kind, pos)))
  }
  def setArity(a: Int): Context = this.copy(procArity = a)

  def addLocal(name: String, kind: DataType, wordSize: Int): Context = {
    val pos = VirtualRegister.StackOffset(frameDepth)
    val nscope = scope.updated(name, Context.LocalVar(kind, pos))

    this.copy(scope = nscope, frameDepth = frameDepth + wordSize)
  }

  def enscope(name: String, value: Context.ScopeObject): Context =
    this.copy(scope = scope.updated(name, value))
}

object Context {
  sealed trait ScopeObject
  case class ProcParam(name: String, index: Int, kind: DataType) extends ScopeObject
  case class LocalVar(kind: DataType, pos: VirtualRegister) extends ScopeObject
  case class Procedure(name: String) extends ScopeObject
  case class DataLabel(kind: DataType, size: Int, ref: LabelRef) extends ScopeObject
}

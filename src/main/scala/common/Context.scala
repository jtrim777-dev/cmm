package com.github.jtrim777.cmm
package common

import lang.DataType

case class Context(scope: Context.Scope, tailPos: Boolean, frameDepth: Int) {
  def setTail: Context = this.copy(tailPos = true)
  def clearTail: Context = this.copy(tailPos = false)

  def enscopeLocal(name: String, kind: DataType, pos: VirtualRegister): Context = {
    val nscope = scope.copy(data = scope.data.updated(name, Context.LocalVar(kind, pos)))

    this.copy(scope = nscope)
  }

  def addLocal(name: String, kind: DataType, wordSize: Int): Context = {
    val pos = VirtualRegister.StackOffset(frameDepth)
    val nscope = scope.copy(data = scope.data.updated(name, Context.LocalVar(kind, pos)))

    this.copy(scope = nscope, frameDepth = frameDepth + wordSize)
  }
}

object Context {
  case class Scope(data: Map[String, ScopeObject])

  sealed trait ScopeObject
  case class LocalVar(kind: DataType, pos: VirtualRegister) extends ScopeObject
  case class Procedure(arguments: Seq[DataType], result: DataType) extends ScopeObject
  case class DataLabel(kind: DataType, size: Int, pos: Int) extends ScopeObject
}

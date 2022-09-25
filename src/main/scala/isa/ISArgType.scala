package dev.jtrim777.cmm
package isa

trait ISArgType[Arch <: ISA] {
  sealed trait Const extends ISArg[Arch]
  sealed trait Callable extends ISArg[Arch]
  sealed trait RMArg extends ISArg[Arch]

  case class Register(index: Int) extends ISArg[Arch] with Callable with RMArg
  case class Constant(value: Long) extends ISArg[Arch] with Const
  case class UConstant(value: Long) extends ISArg[Arch] with Const

  case class LabelRef(name: String) extends ISArg[Arch] with Callable
  object LabelRef {
    def proc(name: String): LabelRef = LabelRef(s"cmm$$proc$$$name")
    def procBody(name: String): LabelRef = LabelRef(s"cmm$$proc_body$$$name")
    def elseCase(id: String): LabelRef = LabelRef(s"cmm$$mark_else$$$id")
    def endIf(id: String): LabelRef = LabelRef(s"cmm$$mark_endif$$$id")
    def localMark(blockId: String, name: String): LabelRef = LabelRef(s"cmm$$block$blockId$$$name")
  }

  case class Offset(base: Register, off: Const) extends ISArg[Arch] with Callable with RMArg
}

package dev.jtrim777.cmm
package asm

sealed trait ISArg {

}

object ISArg {
  sealed trait Const extends ISArg
  sealed trait Callable extends ISArg
  sealed trait RMArg extends ISArg

  case class Register(index: Int) extends ISArg with Callable with RMArg
  case class Constant(value: Long) extends ISArg with Const
  case class UConstant(value: Long) extends ISArg with Const

  case class LabelRef(name: String) extends ISArg with Callable
  object LabelRef {
    def proc(name: String): LabelRef = LabelRef(s"cmm$$proc$$$name")
    def procBody(name: String): LabelRef = LabelRef(s"cmm$$proc_body$$$name")
    def elseCase(id: String): LabelRef = LabelRef(s"cmm$$mark_else$$$id")
    def endIf(id: String): LabelRef = LabelRef(s"cmm$$mark_endif$$$id")
    def localMark(blockId: String, name: String): LabelRef = LabelRef(s"cmm$$block$blockId$$$name")
  }

  case class Offset(base: Register, off: Const) extends ISArg with Callable with RMArg
}

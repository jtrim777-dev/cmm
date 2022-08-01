package dev.jtrim777.cmm
package asm

sealed trait AsmSym {

}

object AsmSym {
  case class Label(name: String) extends AsmSym
  object Label {
    def proc(name: String): Label = Label(s"cmm$$proc$$$name")
    def procBody(name: String): Label = Label(s"cmm$$proc_body$$$name")
    def elseCase(id: String): Label = Label(s"cmm$$mark_else$$$id")
    def endIf(id: String): Label = Label(s"cmm$$mark_endif$$$id")
    def localMark(blockId: String, name: String): Label = Label(s"cmm$$block$blockId$$$name")
  }

  trait Instruction extends AsmSym {
    val mnemonic: String
    val args: Seq[ISArg]
  }
}

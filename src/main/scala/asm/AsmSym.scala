package com.github.jtrim777.cmm
package asm

sealed trait AsmSym {

}

object AsmSym {
  case class Label(name: String) extends AsmSym
  object Label {
    def proc(name: String): Label = Label(s"cmm$$proc$$$name")
    def procBody(name: String): Label = Label(s"cmm$$proc_body$$$name")
  }

  trait Instruction extends AsmSym {
    val mnemonic: String
    val args: Seq[ISArg]
  }
}

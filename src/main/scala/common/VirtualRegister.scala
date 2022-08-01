package dev.jtrim777.cmm
package common

import asm.ISArg.Register
import dev.jtrim777.cmm.asm.{AsmSym, ISArg}

sealed trait VirtualRegister {
}

object VirtualRegister {
  case class Hardware(reg: Register) extends VirtualRegister
  case class StackOffset(bytes: Int) extends VirtualRegister
}

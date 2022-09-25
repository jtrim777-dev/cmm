package dev.jtrim777.cmm
package isa

sealed trait VirtualRegister {

}

object VirtualRegister {
  case class Hardware(reg: ISArg.Register) extends VirtualRegister
  case class StackOffset(bytes: Int) extends VirtualRegister
}

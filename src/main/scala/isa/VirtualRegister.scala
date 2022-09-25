package dev.jtrim777.cmm
package isa

sealed trait VirtualRegister[Arch <: ISA] {

}

object VirtualRegister {
  case class Hardware[Arch <: ISA](reg: Arch#Register) extends VirtualRegister[Arch]
  case class StackOffset[Arch <: ISA](bytes: Int) extends VirtualRegister[Arch]
}

package dev.jtrim777.cmm
package isa.x86_64

import isa.ISArg.Register

object registers {
  val RAX: Register = Register(0)
  val RBX: Register = Register(1)
  val RCX: Register = Register(2)
  val RDX: Register = Register(3)

  val RSI: Register = Register(4)
  val RDI: Register = Register(5)

  val RBP: Register = Register(6)
  val RSP: Register = Register(7)

  val R8: Register = Register(8)
  val R9: Register = Register(9)
  val R10: Register = Register(10)
  val R11: Register = Register(11)
  val R12: Register = Register(12)
  val R13: Register = Register(13)
  val R14: Register = Register(14)
  val R15: Register = Register(15)

  val ArgRegs: List[Register] = List(RDI, RSI, RDX, RCX, R8, R9)
  val MovInterm: Register = R10
  val SecTarget: Register = R11
}

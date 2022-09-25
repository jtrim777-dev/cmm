package dev.jtrim777.cmm
package isa.x86_64

import isa.{ISA, ISArg, ISArgType}

case object X64 extends ISA {
  override type Instr = X64Instr
  override type Arg = ISArg[this.type]
  override val argType: ISArgType[X64.this.type] = new ISArgType[this.type] {}
}
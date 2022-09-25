package dev.jtrim777.cmm
package isa.x86_64

import isa.ISA

case object X64 extends ISA {
  override type Instr = X64Instr
}
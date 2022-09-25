package dev.jtrim777.cmm
package isa

trait ISA { this: ISA =>
  type Instr = Instruction[this.type]
}

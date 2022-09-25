package dev.jtrim777.cmm
package isa

trait ISA { this: ISA =>
  type Instr <: Instruction[this.type]
  type Arg <: ISArg[this.type]
  val argType: ISArgType[this.type]
  type Register = argType.Register
  type Const = argType.Const
  type LabelRef = argType.LabelRef
}

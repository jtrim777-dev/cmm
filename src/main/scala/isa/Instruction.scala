package dev.jtrim777.cmm
package isa

import collection.immutable.Seq

trait Instruction[Arch <: ISA] {
  val mnemonic: String
  val args: Seq[ISArg[Arch]]
}

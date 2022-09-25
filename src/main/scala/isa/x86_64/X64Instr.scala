package dev.jtrim777.cmm
package isa.x86_64

import isa.{ISArg, Instruction}

import OpdSize._
import isa.ISArg._

abstract class X64Instr(suffix: InstrSuffix, val args: ISArg*) extends Instruction[ArchX64] {
  def this(args: ISArg*) = this(BlankSuffix, args:_*)

  override val mnemonic: String = getClass.getSimpleName.stripSuffix("$").toLowerCase + suffix.key
}

object X64Instr {
  case class MOV(src: ISArg, dst: ISArg, size: OpdSize = QuadWord) extends X64Instr(size, src, dst)
  case class MOVZ(src: ISArg, dst: ISArg, srcSize: OpdSize, dstSize: OpdSize) extends X64Instr(srcSize + dstSize, src, dst)
  case class MOVS(src: ISArg, dst: ISArg, srcSize: OpdSize, dstSize: OpdSize) extends X64Instr(srcSize + dstSize, src, dst)
  case class PUSH(src: ISArg, size: OpdSize = QuadWord) extends X64Instr(size, src)
  case class POP(dst: Register, size: OpdSize = QuadWord) extends X64Instr(size, dst)

  case class NEG(target: Register, size: OpdSize = QuadWord) extends X64Instr(size, target)
  case class NOT(target: Register, size: OpdSize = QuadWord) extends X64Instr(size, target)

  case class LEAQ(target: LabelRef, dst: Register) extends X64Instr(target, dst)
  case class ADD(dst: ISArg, src: Register, size: OpdSize = QuadWord) extends X64Instr(size, dst, src)
  case class SUB(dst: ISArg, src: Register, size: OpdSize = QuadWord) extends X64Instr(size, dst, src)
  case class IMUL(dst: ISArg, src: Register, size: OpdSize = QuadWord) extends X64Instr(size, dst, src)
  case class AND(dst: ISArg, src: Register, size: OpdSize = QuadWord) extends X64Instr(size, dst, src)
  case class OR(dst: ISArg, src: Register, size: OpdSize = QuadWord) extends X64Instr(size, dst, src)
  case class XOR(dst: ISArg, src: Register, size: OpdSize = QuadWord) extends X64Instr(size, dst, src)

  case class MUL(op: RMArg, size: OpdSize = QuadWord) extends X64Instr(size, op)
  case class DIV(op: RMArg, size: OpdSize = QuadWord) extends X64Instr(size, op)

  // NOTE: count may ONLY be a const or the register CL (lowest 8 bits of RCX)
  case class SHL(count: ISArg, dst: Register, size: OpdSize = QuadWord) extends X64Instr(size, count, dst)
  case class SHR(count: ISArg, dst: Register, size: OpdSize = QuadWord) extends X64Instr(size, count, dst)
  case class SAR(count: ISArg, dst: Register, size: OpdSize = QuadWord) extends X64Instr(size, count, dst)

  case class CALL(target: Callable) extends X64Instr(target)
  case class RET() extends X64Instr()

  case class CMP(arg1: ISArg, arg2: ISArg, size: OpdSize = QuadWord) extends X64Instr(size, arg1, arg2)
  case class TEST(arg1: ISArg, arg2: ISArg, size: OpdSize = QuadWord) extends X64Instr(size, arg1, arg2)

  case class JMP(target: Callable) extends X64Instr(target)
  case class J(cond: InstrCond, target: LabelRef) extends X64Instr(cond, target)

  case object NOP extends X64Instr()
}

package dev.jtrim777.cmm
package isa.x86_64

import isa.Instruction
import OpdSize._
import X64.Arg

abstract class X64Instr(suffix: InstrSuffix, val args: Arg*) extends Instruction[ArchX64] {
  def this(args: Arg*) = this(BlankSuffix, args:_*)

  override val mnemonic: String = getClass.getSimpleName.stripSuffix("$").toLowerCase + suffix.key
}

object X64Instr {
  import X64.argType._

  case class MOV(src: Arg, dst: Arg, size: OpdSize = QuadWord) extends X64Instr(size, src, dst)
  case class MOVZ(src: Arg, dst: Arg, srcSize: OpdSize, dstSize: OpdSize) extends X64Instr(srcSize + dstSize, src, dst)
  case class MOVS(src: Arg, dst: Arg, srcSize: OpdSize, dstSize: OpdSize) extends X64Instr(srcSize + dstSize, src, dst)
  case class PUSH(src: Arg, size: OpdSize = QuadWord) extends X64Instr(size, src)
  case class POP(dst: Register, size: OpdSize = QuadWord) extends X64Instr(size, dst)

  case class NEG(target: Register, size: OpdSize = QuadWord) extends X64Instr(size, target)
  case class NOT(target: Register, size: OpdSize = QuadWord) extends X64Instr(size, target)

  case class LEAQ(target: LabelRef, dst: Register) extends X64Instr(target, dst)
  case class ADD(dst: Arg, src: Register, size: OpdSize = QuadWord) extends X64Instr(size, dst, src)
  case class SUB(dst: Arg, src: Register, size: OpdSize = QuadWord) extends X64Instr(size, dst, src)
  case class IMUL(dst: Arg, src: Register, size: OpdSize = QuadWord) extends X64Instr(size, dst, src)
  case class AND(dst: Arg, src: Register, size: OpdSize = QuadWord) extends X64Instr(size, dst, src)
  case class OR(dst: Arg, src: Register, size: OpdSize = QuadWord) extends X64Instr(size, dst, src)
  case class XOR(dst: Arg, src: Register, size: OpdSize = QuadWord) extends X64Instr(size, dst, src)

  case class MUL(op: RMArg, size: OpdSize = QuadWord) extends X64Instr(size, op)
  case class DIV(op: RMArg, size: OpdSize = QuadWord) extends X64Instr(size, op)

  case class SHL(count: Const, dst: Register, size: OpdSize = QuadWord) extends X64Instr(size, count, dst)
  case class SHR(count: Const, dst: Register, size: OpdSize = QuadWord) extends X64Instr(size, count, dst)
  case class SAR(count: Const, dst: Register, size: OpdSize = QuadWord) extends X64Instr(size, count, dst)

  case class CALL(target: Callable) extends X64Instr(target)
  case class RET() extends X64Instr()

  case class CMP(arg1: Arg, arg2: Arg, size: OpdSize = QuadWord) extends X64Instr(size, arg1, arg2)
  case class TEST(arg1: Arg, arg2: Arg, size: OpdSize = QuadWord) extends X64Instr(size, arg1, arg2)

  case class JMP(target: Callable) extends X64Instr(target)
  case class J(cond: InstrCond, target: LabelRef) extends X64Instr(cond, target)

  case object NOP extends X64Instr()
}

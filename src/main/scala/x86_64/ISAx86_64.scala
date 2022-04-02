package com.github.jtrim777.cmm
package x86_64

import asm.AsmSym.Instruction
import asm.ISArg
import asm.ISArg._

sealed abstract class ISAx86_64(suffix: String, val args: ISArg*) extends Instruction {
  override val mnemonic: String = getClass.getSimpleName.stripSuffix("$").toLowerCase + suffix
}

object ISAx86_64 {
  sealed abstract class Size(val flag: String)
  case object Byt extends Size("b")
  case object Word extends Size("w")
  case object DblWord extends Size("l")
  case object QuadWord extends Size("q")

  sealed abstract class Cond(val flag: String)
  case object Eq extends Size("e")
  case object NEq extends Size("ne")
  case object Lt extends Size("l")
  case object LEq extends Size("le")
  case object Gt extends Size("g")
  case object GEq extends Size("ge")
  case object Zero extends Size("z")
  case object NZero extends Size("nz")

  case class MOV(src: ISArg, dst: ISArg, size: Size = QuadWord) extends ISAx86_64(size.flag, src, dst)
  case class MOVZ(src: ISArg, dst: ISArg, srcSize: Size, dstSize: Size) extends ISAx86_64(srcSize.flag + dstSize.flag, src, dst)
  case class MOVS(src: ISArg, dst: ISArg, srcSize: Size, dstSize: Size) extends ISAx86_64(srcSize.flag + dstSize.flag, src, dst)
  case class PUSH(src: ISArg, size: Size = QuadWord) extends ISAx86_64(size.flag, src)
  case class POP(dst: Register, size: Size = QuadWord) extends ISAx86_64(size.flag, dst)

  case class NEG(target: Register, size: Size = QuadWord) extends ISAx86_64(size.flag, target)
  case class NOT(target: Register, size: Size = QuadWord) extends ISAx86_64(size.flag, target)

  case class LEAQ(target: LabelRef, dst: Register) extends ISAx86_64("", target, dst)
  case class ADD(src: ISArg, dst: Register, size: Size = QuadWord) extends ISAx86_64(size.flag, src, dst)
  case class SUB(src: ISArg, dst: Register, size: Size = QuadWord) extends ISAx86_64(size.flag, src, dst)
  case class IMUL(src: ISArg, dst: Register, size: Size = QuadWord) extends ISAx86_64(size.flag, src, dst)
  case class AND(src: ISArg, dst: Register, size: Size = QuadWord) extends ISAx86_64(size.flag, src, dst)
  case class OR(src: ISArg, dst: Register, size: Size = QuadWord) extends ISAx86_64(size.flag, src, dst)
  case class XOR(src: ISArg, dst: Register, size: Size = QuadWord) extends ISAx86_64(size.flag, src, dst)

  case class MUL(op: Register, size: Size = QuadWord) extends ISAx86_64(size.flag, op)
  case class DIV(op: Register, size: Size = QuadWord) extends ISAx86_64(size.flag, op)

  case class SHL(count: Const, dst: Register, size: Size = QuadWord) extends ISAx86_64(size.flag, count, dst)
  case class SHR(count: Const, dst: Register, size: Size = QuadWord) extends ISAx86_64(size.flag, count, dst)
  case class SAR(count: Const, dst: Register, size: Size = QuadWord) extends ISAx86_64(size.flag, count, dst)

  case class CALL(target: Callable) extends ISAx86_64("", target)
  case class RET() extends ISAx86_64("")

  case class CMP(arg1: ISArg, arg2: ISArg, size: Size = QuadWord) extends ISAx86_64(size.flag, arg1, arg2)
  case class TEST(arg1: ISArg, arg2: ISArg, size: Size = QuadWord) extends ISAx86_64(size.flag, arg1, arg2)

  case class JMP(target: Callable) extends ISAx86_64("", target)
  case class J(cond: Cond, target: LabelRef) extends ISAx86_64(cond.flag, target)

  case object NOP extends ISAx86_64("")
}



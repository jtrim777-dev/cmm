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
  case object Eq extends Cond("e")
  case object NEq extends Cond("ne")
  case object Lt extends Cond("l")
  case object LEq extends Cond("le")
  case object Gt extends Cond("g")
  case object GEq extends Cond("ge")
  case object Zero extends Cond("z")
  case object NZero extends Cond("nz")
  case object Above extends Cond("a")
  case object NAbove extends Cond("na")
  case object Below extends Cond("b")
  case object NBelow extends Cond("nb")

  case class MOV(src: ISArg, dst: ISArg, size: Size = QuadWord) extends ISAx86_64(size.flag, src, dst)
  case class MOVZ(src: ISArg, dst: ISArg, srcSize: Size, dstSize: Size) extends ISAx86_64(srcSize.flag + dstSize.flag, src, dst)
  case class MOVS(src: ISArg, dst: ISArg, srcSize: Size, dstSize: Size) extends ISAx86_64(srcSize.flag + dstSize.flag, src, dst)
  case class PUSH(src: ISArg, size: Size = QuadWord) extends ISAx86_64(size.flag, src)
  case class POP(dst: Register, size: Size = QuadWord) extends ISAx86_64(size.flag, dst)

  case class NEG(target: Register, size: Size = QuadWord) extends ISAx86_64(size.flag, target)
  case class NOT(target: Register, size: Size = QuadWord) extends ISAx86_64(size.flag, target)

  case class LEAQ(target: LabelRef, dst: Register) extends ISAx86_64("", target, dst)
  case class ADD(dst: ISArg, src: Register, size: Size = QuadWord) extends ISAx86_64(size.flag, dst, src)
  case class SUB(dst: ISArg, src: Register, size: Size = QuadWord) extends ISAx86_64(size.flag, dst, src)
  case class IMUL(dst: ISArg, src: Register, size: Size = QuadWord) extends ISAx86_64(size.flag, dst, src)
  case class AND(dst: ISArg, src: Register, size: Size = QuadWord) extends ISAx86_64(size.flag, dst, src)
  case class OR(dst: ISArg, src: Register, size: Size = QuadWord) extends ISAx86_64(size.flag, dst, src)
  case class XOR(dst: ISArg, src: Register, size: Size = QuadWord) extends ISAx86_64(size.flag, dst, src)

  case class MUL(op: RMArg, size: Size = QuadWord) extends ISAx86_64(size.flag, op)
  case class DIV(op: RMArg, size: Size = QuadWord) extends ISAx86_64(size.flag, op)

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



package dev.jtrim777.cmm
package isa.x86_64

import compile.CompilationContext
import isa.x86_64.registers._
import isa.{ISADSL, ISArg, ISeq, VirtualRegister}
import lang.{DataType, Expression}

object dsl extends ISADSL[ArchX64] {
  import X64.argType._
  import X64Instr.{MOV, MOVS, MOVZ, PUSH}

  override protected def resolveVirtualRegister(vr: VirtualRegister[ArchX64]): ISArg[ArchX64] = vr match {
    case VirtualRegister.Hardware(reg) => reg
    case VirtualRegister.StackOffset(bytes) => Offset(RBP, Constant(bytes))
  }

  override def mov(src: Value, dst: ISArg[ArchX64]): Instrs = src._1 match {
    case r:Register => MOV(r, dst, src._2.operandSize).wrap
    case o:Offset => dst match {
      case r: Register => MOV(o, r, src._2.operandSize).wrap
      case o2: Offset => ISeq(
        MOV(o, MovInterm, src._2.operandSize),
        MOV(MovInterm, o2, src._2.operandSize)
      )
    }
  }

  override def virtMov(src: VirtValue, dst: ISArg[ArchX64]): Instrs = mov((src._1.resolve, src._2), dst)

  override def virtMov(src: Value, dst: VirtualRegister[ArchX64]): Instrs = mov(src, dst.resolve)

  override def typedMov(from: Value, to: Value, sign: Boolean): Instrs = {
    if (from._2.superName != to._2.superName || from._2.bytes > to._2.bytes) {
      throw new IllegalArgumentException(s"Cannot mov ${from._2} to ${to._2}")
    }

    val mover: (X64.Arg, X64.Arg, OpdSize, OpdSize) => X64Instr = if (sign) MOVS.apply else MOVZ.apply

    if (from._2 == to._2) {
      mov(from, to._1)
    } else {
      val ss = from._2.operandSize
      val ds = to._2.operandSize

      from._1 match {
        case r:Register => mover(r, to._1, ss, ds).wrap
        case o:Offset => to._1 match {
          case r: Register => mover(o, r, ss, ds).wrap
          case o2: Offset => mover(o, MovInterm, ss, ds) + MOV(MovInterm, o2, size = ds)
        }
      }
    }
  }

  override def push(arg: ISArg[ArchX64]): Instrs = arg match {
    case r: Register => PUSH(r).wrap
    case other => MOV(other, MovInterm) + PUSH(MovInterm)
  }

  override def fxArg(argPos: Int): VirtualRegister[ArchX64] = {
    if (argPos < 6) {
      VirtualRegister.Hardware(ArgRegs(argPos))
    } else {
      VirtualRegister.StackOffset(8 * (argPos - 4)) // args[6] (the 7th arg) is at RBP + 16
    }
  }

  override protected def makeConstant(value: Long): ArchX64#Const = Constant(value)

  override protected def makeUnsignedConstant(value: Long): ArchX64#Const = UConstant(value)

  override protected def argFromImmExpr(expr: Expression, ctx: CompilationContext[ArchX64]): Value = expr match {
    case Expression.CInt(value) => (value.const, DataType.Word8)
    case Expression.CFlot(_) => throw new IllegalArgumentException("Error compiling float literal: Floats are not yet supported")
    case Expression.ID(name) => ctx.scope(name) match {
      case CompilationContext.ProcParam(_, index, kind) => (fxArg(index).resolve, kind)
      case CompilationContext.LocalVar(kind, pos) => (pos.resolve, kind)
      case CompilationContext.Procedure(name) => (LabelRef.proc(name), DataType.Word8)
      case CompilationContext.DataLabel(_, _, ref) => (ref, DataType.Word8)
    }
    case _ => throw new IllegalArgumentException(s"Error compiling expression: $expr is not immediate")
  }

  implicit class DTOps(dt: DataType) {
    import OpdSize._
    def operandSize: OpdSize = dt.bytes match {
      case 1 => Byt
      case 2 => Word
      case 4 => DblWord
      case 8 => QuadWord
    }
  }
}

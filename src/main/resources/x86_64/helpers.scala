package dev.jtrim777.cmm
package x86_64

import asm.ISArg.{LabelRef, Register}
import asm.{AsmSym, ISArg}
import lang.{DataType, Expression, RelOp}
import x86_64.ISAx86_64._
import x86_64.registers._
import dev.jtrim777.cmm.compile.CompilationContext

import dev.jtrim777.cmm.isa.VirtualRegister
import dev.jtrim777.cmm.isa.x86_64.ArchX64

object helpers {
  implicit class VRHelper(vr: VirtualRegister) {
    def asArg: ISArg = vr match {
      case VirtualRegister.Hardware(reg) => reg
      case VirtualRegister.StackOffset(bytes) => ISArg.Offset(RBP, ISArg.Constant(bytes))
    }
  }

  implicit class DTHelper(dt: DataType) {
    def asSize: Size = dt.bytes match {
      case 1 => Byt
      case 2 => Word
      case 4 => DblWord
      case 8 => QuadWord
    }
  }

  private val Args6: Array[Register] = Array(
    RDI, RSI, RDX, RCX, R8, R9
  )

  /*
  Arg 1: RDI
  Arg 2: RSI
  Arg 3: RDX
  Arg 4: RCX
  Arg 5: R8
  Arg 6: R9
  Args 7+: RBP - 8(i + 1)
   */
  def argPosToVR(ind: Int): VirtualRegister[ArchX64] = {
    if (ind < 6) {
      VirtualRegister.Hardware[ArchX64](Args6(ind))
    } else {
      VirtualRegister.StackOffset(8 * (ind - 4)) // args[6] (the 7th arg) is at RBP + 16
    }
  }

  def saveMov(src: ISArg, dst: ISArg, kind: DataType): Seq[AsmSym] = src match {
    case r:Register => Seq(MOV(r, dst, kind.asSize))
    case o:ISArg.Offset => dst match {
      case r: Register => Seq(MOV(o, r, kind.asSize))
      case o2: ISArg.Offset => Seq(
        MOV(o, MovInterm, kind.asSize),
        MOV(MovInterm, o2, kind.asSize)
      )
    }
  }

  def movTo(virt: VirtualRegister, src: ISArg, kind: DataType): Seq[AsmSym] =
    saveMov(src, virt.asArg, kind)

  def movFrom(virt: VirtualRegister, dst: ISArg, kind: DataType): Seq[AsmSym] =
    saveMov(virt.asArg, dst, kind)

  def typedMov(from: (ISArg, DataType), to: (ISArg, DataType), sign: Boolean = false): Seq[AsmSym] = {
    if (from._2.superName != to._2.superName || from._2.bytes > to._2.bytes) {
      throw new IllegalArgumentException(s"Cannot mov ${from._2} to ${to._2}")
    }

    val mover: (ISArg, ISArg, Size, Size) => ISAx86_64 = if (sign) MOVS.apply else MOVZ.apply

    if (from._2 == to._2) {
      saveMov(from._1, to._1, to._2)
    } else {
      val ss = from._2.asSize
      val ds = to._2.asSize

      from._1 match {
        case r:Register => Seq(mover(r, to._1, ss, ds))
        case o:ISArg.Offset => to._1 match {
          case r: Register => Seq(mover(o, r, ss, ds))
          case o2: ISArg.Offset => Seq(
            mover(o, MovInterm, ss, ds),
            MOV(MovInterm, o2, size = ds)
          )
        }
      }
    }
  }

  def safePush(src: ISArg): Seq[AsmSym] = src match {
    case r:Register => Seq(PUSH(r))
    case other => Seq(MOV(other, MovInterm), PUSH(MovInterm))
  }

  def findParameters(expr: Expression, ctx: CompilationContext): Seq[CompilationContext.ProcParam] = expr match {
    case Expression.ID(name) => ctx.scope.get(name).collect {
      case p:CompilationContext.ProcParam => Seq(p)
    }.getOrElse(Seq.empty)
    case Expression.Read(_, pos, _) => findParameters(pos, ctx)
    case Expression.InfixOp(lhs, _, rhs) => findParameters(lhs, ctx) ++ findParameters(rhs, ctx)
    case Expression.PrefixOp(_, target) => findParameters(target, ctx)
    case Expression.Operation(_, args) => args.flatMap(findParameters(_, ctx))
  }

  implicit class NumSyntax(num: Int) {
    def const: ISArg.Const = ISArg.Constant(num)
    def uconst: ISArg.Const = ISArg.UConstant(num)
  }

  implicit class LongNumSyntax(num: Long) {
    def const: ISArg.Const = ISArg.Constant(num)
    def uconst: ISArg.Const = ISArg.UConstant(num)
  }

  def argFromImmediate(expr: Expression, ctx: CompilationContext): (ISArg, DataType) = expr match {
    case Expression.CInt(value) => (value.const, DataType.Word8)
    case Expression.CFlot(_) => throw new IllegalArgumentException("Error compiling float literal: Floats are not yet supported")
    case Expression.ID(name) => ctx.scope(name) match {
      case CompilationContext.ProcParam(_, index, kind) => (argPosToVR(index).asArg, kind)
      case CompilationContext.LocalVar(kind, pos) => (pos.asArg, kind)
      case CompilationContext.Procedure(name) => (LabelRef.proc(name), DataType.Word8)
      case CompilationContext.DataLabel(_, _, ref) => (ref, DataType.Word8)
    }
    case _ => throw new IllegalArgumentException(s"Error compiling expression: $expr is not immediate")
  }
}

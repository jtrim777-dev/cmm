package com.github.jtrim777.cmm
package x86_64

import asm.ISArg.Register
import asm.{AsmSym, ISArg}
import common.VirtualRegister
import lang.{DataType, RelOp}
import x86_64.ISAx86_64._
import x86_64.registers._

object helpers {
  implicit class SOHelper(so: VirtualRegister.StackOffset) {
    def asArg: ISArg = ISArg.Offset(RBP, ISArg.Constant(so.bytes))
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
  def argPosToVR(ind: Int): VirtualRegister = {
    if (ind < 6) {
      VirtualRegister.Hardware(Args6(ind))
    } else {
      VirtualRegister.StackOffset(-8 * (ind+1))
    }
  }

  def saveMov(src: ISArg, dst: ISArg, kind: DataType): Seq[AsmSym] = src match {
    case r:Register => Seq(MOV(r, dst, kind.asSize))
    case o:ISArg.Offset => dst match {
      case r: Register => Seq(MOV(o, r, kind.asSize))
      case o2: ISArg.Offset => Seq(
        MOV(o, Scratch1, kind.asSize),
        MOV(Scratch1, o2, kind.asSize)
      )
    }
  }

  def movTo(virt: VirtualRegister, src: ISArg, kind: DataType): Seq[AsmSym] = virt match {
    case VirtualRegister.Hardware(reg) => Seq(MOV(src, reg, kind.asSize))
    case off:VirtualRegister.StackOffset => saveMov(src, off.asArg, kind)
  }

  def movFrom(virt: VirtualRegister, dst: ISArg, kind: DataType): Seq[AsmSym] = virt match {
    case VirtualRegister.Hardware(reg) => Seq(MOV(reg, dst, kind.asSize))
    case off:VirtualRegister.StackOffset => saveMov(off.asArg, dst, kind)
  }

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
            mover(o, Scratch1, ss, ds),
            MOV(Scratch1, o2, size = ds)
          )
        }
      }
    }
  }
}

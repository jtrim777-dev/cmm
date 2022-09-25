package dev.jtrim777.cmm
package isa

import compile.CompilationContext
import lang.{DataType, Expression}
import isa.ISArg._

import collection.immutable.Seq

trait ISADSL[Arch <: ISA] {
  type Value = (ISArg, DataType)
  type VirtValue = (VirtualRegister, DataType)
  type Instrs = ISeq[Arch]

  object Instrs {
    def empty: Instrs = ISeq.empty[Arch]

    def apply(is: Arch#Instr*): Instrs = ISeq.apply[Arch](is:_*)
  }

  protected def resolveVirtualRegister(vr: VirtualRegister): ISArg

  def mov(src: Value, dst: ISArg): Instrs

  def virtMov(src: VirtValue, dst: ISArg): Instrs

  def virtMov(src: Value, dst: VirtualRegister): Instrs

  def typedMov(from: Value, to: Value, sign: Boolean = false): Instrs

  def push(arg: ISArg): Instrs

  def fxArg(argPos: Int): VirtualRegister

  protected def makeConstant(value: Long): Const
  protected def makeUnsignedConstant(value: Long): Const

  protected def argFromImmExpr(expr: Expression, ctx: CompilationContext[Arch]): Value

  def value(arg: ISArg, kind: DataType): Value = (arg, kind)
  def value(arg: VirtualRegister, kind: DataType): VirtValue = (arg, kind)

  implicit class ArgOps(arg: ISArg) {
    def push: Instrs = ISADSL.this.push(arg)
  }

  implicit class ValueOps(v: Value) {
    def moveTo(dst: ISArg): Instrs = ISADSL.this.mov(v, dst)

    def moveTo(dst: Value): Instrs = ISADSL.this.typedMov(v, dst)

    def moveTo(dst: Value, signed: Boolean): Instrs = ISADSL.this.typedMov(v, dst, sign = signed)

    def moveTo(dst: VirtualRegister): Instrs = ISADSL.this.virtMov(v, dst)

    def push: Instrs = ISADSL.this.push(v._1)
  }

  implicit class VROps(vr: VirtualRegister) {
    def resolve: ISArg = resolveVirtualRegister(vr)
  }

  implicit class VirtValueOps(vv: VirtValue) {
    def moveTo(dst: ISArg): Instrs = ISADSL.this.virtMov(vv, dst)
  }

  implicit class NumSyntax(num: Int) {
    def const: Const = ISADSL.this.makeConstant(num)
    def uconst: Const  = ISADSL.this.makeUnsignedConstant(num)
  }

  implicit class LongNumSyntax(num: Long) {
    def const: Const = ISADSL.this.makeConstant(num)
    def uconst: Const = ISADSL.this.makeUnsignedConstant(num)
  }

  implicit class ExprOps(expr: Expression) {
    def asArg(ctx: CompilationContext[Arch]): Value = ISADSL.this.argFromImmExpr(expr, ctx)
  }

  implicit class InstrOps(instr: Arch#Instr) {
    def wrap : Instrs = ISeq(instr)

    def +(other: Arch#Instr): ISeq[Arch] = ISeq(instr, other)
  }
}

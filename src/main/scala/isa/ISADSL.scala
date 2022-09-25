package dev.jtrim777.cmm
package isa

import compile.CompilationContext
import lang.{DataType, Expression}

import collection.immutable.Seq

trait ISADSL[Arch <: ISA] {
  type Value = (Arch#Arg, DataType)
  type VirtValue = (VirtualRegister[Arch], DataType)
  type Instrs = ISeq[Arch]

  object Instrs {
    def empty: Instrs = ISeq.empty[Arch]

    def apply(is: Arch#Instr*): Instrs = ISeq.apply[Arch](is:_*)
  }

  protected def resolveVirtualRegister(vr: VirtualRegister[Arch]): Arch#Arg

  def mov(src: Value, dst: Arch#Arg): Instrs

  def virtMov(src: VirtValue, dst: Arch#Arg): Instrs

  def virtMov(src: Value, dst: VirtualRegister[Arch]): Instrs

  def typedMov(from: Value, to: Value, sign: Boolean = false): Instrs

  def push(arg: Arch#Arg): Instrs

  def fxArg(argPos: Int): VirtualRegister[Arch]

  protected def makeConstant(value: Long): Arch#Const
  protected def makeUnsignedConstant(value: Long): Arch#Const

  protected def argFromImmExpr(expr: Expression, ctx: CompilationContext[Arch]): Value

  def value(arg: Arch#Arg, kind: DataType): Value = (arg, kind)
  def value(arg: VirtualRegister[Arch], kind: DataType): VirtValue = (arg, kind)

  implicit class ArgOps(arg: Arch#Arg) {
    def push: Instrs = ISADSL.this.push(arg)
  }

  implicit class ValueOps(v: Value) {
    def moveTo(dst: Arch#Arg): Instrs = ISADSL.this.mov(v, dst)

    def moveTo(dst: Value): Instrs = ISADSL.this.typedMov(v, dst)

    def moveTo(dst: Value, signed: Boolean): Instrs = ISADSL.this.typedMov(v, dst, sign = signed)

    def moveTo(dst: VirtualRegister[Arch]): Instrs = ISADSL.this.virtMov(v, dst)

    def push: Instrs = ISADSL.this.push(v._1)
  }

  implicit class VROps(vr: VirtualRegister[Arch]) {
    def resolve: Arch#Arg = resolveVirtualRegister(vr)
  }

  implicit class VirtValueOps(vv: VirtValue) {
    def moveTo(dst: Arch#Arg): Instrs = ISADSL.this.virtMov(vv, dst)
  }

  implicit class NumSyntax(num: Int) {
    def const: Arch#Const = ISADSL.this.makeConstant(num)
    def uconst: Arch#Const  = ISADSL.this.makeUnsignedConstant(num)
  }

  implicit class LongNumSyntax(num: Long) {
    def const: Arch#Const = ISADSL.this.makeConstant(num)
    def uconst: Arch#Const = ISADSL.this.makeUnsignedConstant(num)
  }

  implicit class ExprOps(expr: Expression) {
    def asArg(ctx: CompilationContext[Arch]): Value = ISADSL.this.argFromImmExpr(expr, ctx)
  }

  implicit class InstrOps(instr: Arch#Instr) {
    def wrap : Instrs = ISeq(instr)

    def +(other: Arch#Instr): ISeq[Arch] = ISeq(instr, other)
  }
}

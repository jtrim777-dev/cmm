package dev.jtrim777.cmm
package compile.x86_64

import compile.x86_64.CompileX64.Context
import isa.x86_64.dsl._
import isa.x86_64.registers._
import isa.x86_64.X64Instr._
import lang.{DataType, Primitive}
import lang.Primitive._
import compile.{raise, verify}

import cats.data.Kleisli
import cats.effect.IO
import dev.jtrim777.cmm.isa.ISArg
import dev.jtrim777.cmm.isa.ISArg.Register
import dev.jtrim777.cmm.isa.x86_64.{OpdSize, X64Instr}

object operations {
  def getCompiler(op: Primitive): IO[Kleisli[IO, (Seq[Value], Register, Context), Instrs]] = IO(op).map {
    case Add | UAdd | FAdd => Kleisli(t => add(op.unsigned, op.floating)(t._1, t._2, t._3))
    case Sub | USub | FSub => Kleisli(t => sub(op.unsigned, op.floating)(t._1, t._2, t._3))
    case Mul | UMul | FMul => Kleisli(t => mul(op.unsigned, op.floating)(t._1, t._2, t._3))
    case Div | UDiv | FDiv => Kleisli(t => div(op.unsigned, op.floating)(t._1, t._2, t._3))
    case Mod | UMod => Kleisli(t => mod(op.unsigned)(t._1, t._2, t._3))
    case And => Kleisli(t => and(t._1, t._2, t._3))
    case Or => Kleisli(t => or(t._1, t._2, t._3))
    case Xor => Kleisli(t => xor(t._1, t._2, t._3))
    case Not => Kleisli(t => not(t._1, t._2, t._3))
    case ShiftL => Kleisli(t => shiftL(t._1, t._2, t._3))
    case ShiftR => Kleisli(t => shiftR(t._1, t._2, t._3))
    case ShiftRA => Kleisli(t => shiftRA(t._1, t._2, t._3))
    case Abs | FAbs => Kleisli(t => abs(op.floating)(t._1, t._2, t._3))
    case Neg | FNeg => Kleisli(t => neg(op.floating)(t._1, t._2, t._3))
    case Sign | FSign => Kleisli(t => sign(op.floating)(t._1, t._2, t._3))
    case Round => Kleisli(t => round(t._1, t._2, t._3))
    case Trunc => Kleisli(t => trunc(t._1, t._2, t._3))
    case ToWord1 | ToWord2 | ToWord4 | ToWord8 => Kleisli(t => resizeWord(DataType.fromName(op.name).get)(t._1, t._2, t._3))
    case ToFloat4 | ToFloat8 => Kleisli(t => resizeFlot(DataType.fromName(op.name).get)(t._1, t._2, t._3))
    case CastWord => Kleisli(t => castWord(t._1, t._2, t._3))
    case CastFlot => Kleisli(t => castFlot(t._1, t._2, t._3))
    case Eq | FEq => Kleisli(t => equal(op.floating)(t._1, t._2, t._3))
    case NEq | FNEq => Kleisli(t => notEqual(op.floating)(t._1, t._2, t._3))
    case LT | ULT | FLT => Kleisli(t => lessThan(op.unsigned, op.floating)(t._1, t._2, t._3))
    case LTE | ULTE | FLTE => Kleisli(t => lessThanEq(op.unsigned, op.floating)(t._1, t._2, t._3))
    case GT | UGT | FGT => Kleisli(t => greaterThan(op.unsigned, op.floating)(t._1, t._2, t._3))
    case GTE | UGTE | FGTE => Kleisli(t => greaterThanEq(op.unsigned, op.floating)(t._1, t._2, t._3))
  }

  private def equivocateArgs(arg1: Value, arg2: Value, pos1: Register, sign: Boolean): IO[(Instrs, Value)] = {
    if (arg1._2 == arg2._2) {
      IO.pure((arg1 moveTo pos1, arg2))
    } else {
      IO(arg1._2.bounding(arg2._2)).flatMap {
        case Some(v) => IO.pure(v)
        case None => raise("operation arguments", s"${arg1._2} and ${arg2._2} cannot be used together")
      }.flatMap { nt => IO {
        val mv1 = if (arg1._2 == nt) arg1 moveTo pos1 else {
          arg1.moveTo(value(pos1, nt), sign)
        }

        val (mv2, n2) = if (arg2._2 == nt) (Instrs.empty, arg2) else {
          (arg2.moveTo(value(MovInterm, nt), sign), value(MovInterm, nt))
        }

        (mv1 ++ mv2, n2)
      }}
    }
  }

  private def simpleBinary(signed: Boolean, floating: Boolean, args: Seq[Value], target: Register, name: String)
                          (op: (Register, Register, OpdSize) => X64Instr): IO[Instrs] = {
    for {
      _ <- verify(!floating, name + " operation", "Floating point operations are not yet supported")
      xargs <- IO((args.head, args(1)))
      ear <- equivocateArgs(xargs._1, xargs._2, target, signed)
      (argSetup, arg2) = ear
      mv2 <- IO(arg2 moveTo MovInterm)
      lastInst = op(target, MovInterm, arg2._2.operandSize)
    } yield argSetup ++ mv2 + lastInst
  }

  def add(unsigned: Boolean, floating: Boolean)(args: Seq[Value], target: Register, ctx: Context): IO[Instrs] = {
    simpleBinary(!unsigned, floating, args, target, "add")(ADD.apply)
  }

  def sub(unsigned: Boolean, floating: Boolean)(args: Seq[Value], target: Register, ctx: Context): IO[Instrs] = {
    simpleBinary(!unsigned, floating, args, target, "sub")(SUB.apply)
  }

  def mul(unsigned: Boolean, floating: Boolean)(args: Seq[Value], target: Register, ctx: Context): IO[Instrs] = ???

  def div(unsigned: Boolean, floating: Boolean)(args: Seq[Value], target: Register, ctx: Context): IO[Instrs] = ???

  def mod(unsigned: Boolean)(args: Seq[Value], target: Register, ctx: Context): IO[Instrs] = ???

  def and(args: Seq[Value], target: Register, ctx: Context): IO[Instrs] = {
    simpleBinary(signed = false, floating = false, args, target, "and")(AND.apply)
  }

  def or(args: Seq[Value], target: Register, ctx: Context): IO[Instrs] = {
    simpleBinary(signed = false, floating = false, args, target, "or")(OR.apply)
  }

  def xor(args: Seq[Value], target: Register, ctx: Context): IO[Instrs] = {
    simpleBinary(signed = false, floating = false, args, target, "xor")(XOR.apply)
  }

  def not(args: Seq[Value], target: Register, ctx: Context): IO[Instrs] = for {
    arg <- IO(args.head)
    mtt = arg moveTo target
    lastInst = NOT(target, arg._2.operandSize)
  } yield mtt + lastInst

  def prepareShiftValue(raw: Value, ctx: Context): IO[(Instrs, ISArg, Boolean)] = {
    val mustSaveRCX = ctx.procArity >= 4 // RCX being used as a procedure argument

    

    raw._1 match {
      case Register(index) => ???
      case ISArg.Constant(value) => IO.pure((Instrs.empty, ISArg.Constant(Math.abs(value) & 0xFF), value < 0))
      case ISArg.UConstant(value) => IO.pure((Instrs.empty, ISArg.UConstant(value & 0xFF), false))
      case ISArg.LabelRef(name) => ???
      case ISArg.Offset(base, off) => ???
    }
  }

  def shiftL(args: Seq[Value], target: Register, ctx: Context): IO[Instrs] = ???

  def shiftR(args: Seq[Value], target: Register, ctx: Context): IO[Instrs] = ???

  def shiftRA(args: Seq[Value], target: Register, ctx: Context): IO[Instrs] = ???

  def abs(floating: Boolean)(args: Seq[Value], target: Register, ctx: Context): IO[Instrs] = ???

  def neg(floating: Boolean)(args: Seq[Value], target: Register, ctx: Context): IO[Instrs] = ???

  def sign(floating: Boolean)(args: Seq[Value], target: Register, ctx: Context): IO[Instrs] = ???

  def round(args: Seq[Value], target: Register, ctx: Context): IO[Instrs] = ???

  def trunc(args: Seq[Value], target: Register, ctx: Context): IO[Instrs] = ???

  def resizeWord(tt: DataType)(args: Seq[Value], target: Register, ctx: Context): IO[Instrs] = ???

  def resizeFlot(tt: DataType)(args: Seq[Value], target: Register, ctx: Context): IO[Instrs] = ???

  def castWord(args: Seq[Value], target: Register, ctx: Context): IO[Instrs] = ???

  def castFlot(args: Seq[Value], target: Register, ctx: Context): IO[Instrs] = ???

  def equal(floating: Boolean)(args: Seq[Value], target: Register, ctx: Context): IO[Instrs] = ???

  def notEqual(floating: Boolean)(args: Seq[Value], target: Register, ctx: Context): IO[Instrs] = ???

  def lessThan(unsigned: Boolean, floating: Boolean)(args: Seq[Value], target: Register, ctx: Context): IO[Instrs] = ???

  def lessThanEq(unsigned: Boolean, floating: Boolean)(args: Seq[Value], target: Register, ctx: Context): IO[Instrs] = ???

  def greaterThan(unsigned: Boolean, floating: Boolean)(args: Seq[Value], target: Register, ctx: Context): IO[Instrs] = ???

  def greaterThanEq(unsigned: Boolean, floating: Boolean)(args: Seq[Value], target: Register, ctx: Context): IO[Instrs] = ???

}

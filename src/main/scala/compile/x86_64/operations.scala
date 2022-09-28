package dev.jtrim777.cmm
package compile.x86_64

import compile.x86_64.CompileX64.Context
import compile.{raise, verify}
import isa.ISArg
import isa.ISArg.Register
import isa.x86_64.X64Instr._
import isa.x86_64.dsl._
import isa.x86_64.registers._
import isa.x86_64.{InstrCond, OpdSize, X64Instr}
import lang.Primitive._
import lang.{DataType, Primitive}

import cats.data.Kleisli
import cats.effect.IO

object operations {
  def getCompiler(op: Primitive): IO[Kleisli[IO, (Seq[Value], Register, Context), Instrs]] = IO(op).map {
    case Add | UAdd | FAdd => Kleisli(t => add(op.unsigned, op.floating)(t._1, t._2))
    case Sub | USub | FSub => Kleisli(t => sub(op.unsigned, op.floating)(t._1, t._2))
    case Mul | UMul | FMul => Kleisli(t => mul(op.unsigned, op.floating)(t._1, t._2, t._3))
    case Div | UDiv | FDiv => Kleisli(t => div(op.unsigned, op.floating)(t._1, t._2, t._3))
    case Mod | UMod => Kleisli(t => mod(op.unsigned)(t._1, t._2, t._3))
    case And => Kleisli(t => and(t._1, t._2))
    case Or => Kleisli(t => or(t._1, t._2))
    case Xor => Kleisli(t => xor(t._1, t._2))
    case Not => Kleisli(t => not(t._1, t._2))
    case ShiftL => Kleisli(t => shiftL(t._1, t._2, t._3))
    case ShiftR => Kleisli(t => shiftR(t._1, t._2, t._3))
    case ShiftRA => Kleisli(t => shiftRA(t._1, t._2, t._3))
    case Abs | FAbs => Kleisli(t => abs(op.floating)(t._1, t._2))
    case Neg | FNeg => Kleisli(t => neg(op.floating)(t._1, t._2))
    case Sign | FSign => Kleisli(t => sign(op.floating)(t._1, t._2))
    case Round => Kleisli(t => round(t._1, t._2, t._3))
    case Trunc => Kleisli(t => trunc(t._1, t._2, t._3))
    case ToWord1 | ToWord2 | ToWord4 | ToWord8 => Kleisli(t => resizeWord(DataType.fromName(op.name).get)(t._1, t._2))
    case ToFloat4 | ToFloat8 => Kleisli(t => resizeFlot(DataType.fromName(op.name).get)(t._1, t._2, t._3))
    case CastWord => Kleisli(t => castWord(t._1, t._2, t._3))
    case CastFlot => Kleisli(t => castFlot(t._1, t._2, t._3))
    case Eq | FEq => Kleisli(t => equal(op.floating)(t._1, t._2))
    case NEq | FNEq => Kleisli(t => notEqual(op.floating)(t._1, t._2))
    case LT | ULT | FLT => Kleisli(t => lessThan(op.unsigned, op.floating)(t._1, t._2))
    case LTE | ULTE | FLTE => Kleisli(t => lessThanEq(op.unsigned, op.floating)(t._1, t._2))
    case GT | UGT | FGT => Kleisli(t => greaterThan(op.unsigned, op.floating)(t._1, t._2))
    case GTE | UGTE | FGTE => Kleisli(t => greaterThanEq(op.unsigned, op.floating)(t._1, t._2))
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

  private def binarySetup(signed: Boolean, floating: Boolean, args: Seq[Value],
                          target: Register, name: String): IO[(Instrs, Value)] = for {
    _ <- verify(!floating, name + " operation", "Floating point operations are not yet supported")
    xargs <- IO((args.head, args(1)))
    ear <- equivocateArgs(xargs._1, xargs._2, target, signed)
  } yield ear

  private def simpleBinary(signed: Boolean, floating: Boolean, args: Seq[Value], target: Register, name: String)
                          (op: (Register, Register, OpdSize) => X64Instr): IO[Instrs] = {
    for {
      ear <- binarySetup(signed, floating, args, target, name)
      (argSetup, arg2) = ear
      mv2 <- IO(arg2 moveTo MovInterm)
      lastInst = op(target, MovInterm, arg2._2.operandSize)
    } yield argSetup ++ mv2 + lastInst
  }

  def add(unsigned: Boolean, floating: Boolean)(args: Seq[Value], target: Register): IO[Instrs] = {
    simpleBinary(!unsigned, floating, args, target, "add")(ADD.apply)
  }

  def sub(unsigned: Boolean, floating: Boolean)(args: Seq[Value], target: Register): IO[Instrs] = {
    simpleBinary(!unsigned, floating, args.reverse, target, "sub")(SUB.apply) // Sub works as dst - src
  }

  private def complexSettings(target: Register, ctx: Context): IO[(Instrs, Instrs)] = for {
    pushD <- IO(if (ctx.procArity >= 3) PUSH(RDX).wrap else Instrs.empty)
    pushA = if (target != RAX) PUSH(RAX) +: value(target, DataType.Word8).moveTo(RAX) else Instrs.empty
    pushedA = pushA.nonEmpty
    pushedD = pushD.nonEmpty
    cleanup = (if (pushedA) POP(RAX).wrap else Instrs.empty) ++ (if (pushedD) POP(RDX).wrap else Instrs.empty)
  } yield (pushD ++ pushA, cleanup)

  private def fullMul(target: Register, arg2: ISArg, signed: Boolean, ctx: Context): IO[Instrs] = for {
    scsr <- complexSettings(target, ctx)
    (setup, cleanup) = scsr
    cmd = if (signed) IMULQ.apply _ else MULQ.apply _
    doit = cmd(arg2).wrap
    setTarget = if (target != RAX) MOV(RAX, target).wrap else Instrs.empty
  } yield setup ++ doit ++ setTarget ++ cleanup

  private def simpleMul(target: Register, arg2: ISArg, kind: DataType): IO[Instrs] = IO {
    IMUL(arg2, target, kind.operandSize).wrap
  }

  def mul(unsigned: Boolean, floating: Boolean)(args: Seq[Value], target: Register, ctx: Context): IO[Instrs] = {
    for {
      ear <- binarySetup(!unsigned, floating, args, target, "multiply")
      (setup, (arg2, kind)) = ear
      fx <- if (kind == DataType.Word8) fullMul(target, arg2, !unsigned, ctx) else simpleMul(target, arg2, kind)
    } yield setup ++ fx
  }

  private def doDiv(target: Register, arg2: ISArg, signed: Boolean, ctx: Context, rez: Register): IO[Instrs] = for {
    scsr <- complexSettings(target, ctx)
    (setup, cleanup) = scsr
    cmd = if (signed) IDIVQ.apply _ else DIVQ.apply _
    setD = CQTO().wrap
    doit = cmd(arg2).wrap
    moveTarget = if (target != rez) MOV(rez, target).wrap else Instrs.empty
  } yield setup ++ setD ++ doit ++ moveTarget ++ cleanup

  def div(unsigned: Boolean, floating: Boolean)(args: Seq[Value], target: Register, ctx: Context): IO[Instrs] = for {
    ear <- binarySetup(!unsigned, floating, args, target, "divide")
    (setup, (arg2, _)) = ear
    process <- doDiv(target, arg2, !unsigned, ctx, RAX) // Quotient of divide is in RAX
  } yield setup ++ process

  def mod(unsigned: Boolean)(args: Seq[Value], target: Register, ctx: Context): IO[Instrs] = for {
    ear <- binarySetup(!unsigned, floating = false, args, target, "modulo")
    (setup, (arg2, _)) = ear
    process <- doDiv(target, arg2, !unsigned, ctx, RDX) // Remainder of divide is in RDX
  } yield setup ++ process

  def and(args: Seq[Value], target: Register): IO[Instrs] = {
    simpleBinary(signed = false, floating = false, args, target, "and")(AND.apply)
  }

  def or(args: Seq[Value], target: Register): IO[Instrs] = {
    simpleBinary(signed = false, floating = false, args, target, "or")(OR.apply)
  }

  def xor(args: Seq[Value], target: Register): IO[Instrs] = {
    simpleBinary(signed = false, floating = false, args, target, "xor")(XOR.apply)
  }

  def not(args: Seq[Value], target: Register): IO[Instrs] = for {
    arg <- IO(args.head)
    mtt = arg moveTo target
    lastInst = NOT(target, arg._2.operandSize)
  } yield mtt + lastInst

  /**
   * Setup count argument to shift instr. Arg must be constant or a byte in RCX
   * @return (The setup instrs, the argument to pass to shift, whether or not to flip the shift)
   */
  private def prepareShiftValue(raw: Value, ctx: Context): IO[(Instrs, ISArg, Boolean)] = {
    val mustSaveRCX = ctx.procArity >= 4 // RCX being used as a procedure argument

    def prepareFromRegister(r: Register): IO[Instrs] = IO {
      val p1 = if (mustSaveRCX) PUSH(RCX).wrap else Instrs.empty

      value(r, raw._2).moveTo(value(RCX, DataType.Word1)) ++ p1
    }

    raw._1 match {
      case ISArg.Constant(value) => IO.pure((Instrs.empty, ISArg.Constant(Math.abs(value) & 0xFF), value < 0))
      case ISArg.UConstant(value) => IO.pure((Instrs.empty, ISArg.UConstant(value & 0xFF), false))
      case _ => prepareFromRegister(MovInterm)
        .map(i => (raw.moveTo(MovInterm) ++ i, RCX, false))
    }
  }

  private def shift(args: Seq[Value], target: Register, ctx: Context)
                   (instr: (ISArg, Register, OpdSize) => X64Instr,
                    opp: (ISArg, Register, OpdSize) => X64Instr): IO[Instrs] = {
    for {
      argPair <- IO((args.head, args(1)))
      (arg1, arg2) = argPair
      psv <- prepareShiftValue(arg2, ctx)
      (setup, countArg, flip) = psv
      rinstr = if (flip) opp else instr
      iapp = rinstr(countArg, target, arg1._2.operandSize).wrap
      cleanup = if (setup.contains(PUSH(RCX))) POP(RCX).wrap else Instrs.empty
    } yield setup ++ (arg1 moveTo target) ++ iapp ++ cleanup
  }

  def shiftL(args: Seq[Value], target: Register, ctx: Context): IO[Instrs] = shift(args, target, ctx)(SHL, SHR)

  def shiftR(args: Seq[Value], target: Register, ctx: Context): IO[Instrs] = shift(args, target, ctx)(SHR, SHL)

  def shiftRA(args: Seq[Value], target: Register, ctx: Context): IO[Instrs] = shift(args, target, ctx)(SAR, SHL)

  private def comparison(floating: Boolean, signed: Boolean, args: Seq[Value], target: Register)
                        (uop: InstrCond, sop: InstrCond): IO[Instrs] = for {
    _ <- verify(!floating, "comparison operation", "floating point operations are not yet supported")
    xargs <- IO((args.head, args(1)))
    ear <- equivocateArgs(xargs._1, xargs._2, target, signed)
    (setup, arg2) = ear
    cmp = CMP(arg2._1, target, arg2._2.operandSize)
    fin = SET(if (signed) sop else uop, target)
  } yield (setup + cmp) + fin

  def equal(floating: Boolean)(args: Seq[Value], target: Register): IO[Instrs] =
    comparison(floating, signed = false, args, target)(InstrCond.Eq, InstrCond.Eq)

  def notEqual(floating: Boolean)(args: Seq[Value], target: Register): IO[Instrs] =
    comparison(floating, signed = false, args, target)(InstrCond.NEq, InstrCond.NEq)

  def lessThan(unsigned: Boolean, floating: Boolean)(args: Seq[Value], target: Register): IO[Instrs] =
    comparison(floating, !unsigned, args, target)(InstrCond.Lt, InstrCond.Below)

  def lessThanEq(unsigned: Boolean, floating: Boolean)(args: Seq[Value], target: Register): IO[Instrs] =
    comparison(floating, !unsigned, args, target)(InstrCond.LEq, InstrCond.NAbove)

  def greaterThan(unsigned: Boolean, floating: Boolean)(args: Seq[Value], target: Register): IO[Instrs] =
    comparison(floating, !unsigned, args, target)(InstrCond.Gt, InstrCond.Above)

  def greaterThanEq(unsigned: Boolean, floating: Boolean)(args: Seq[Value], target: Register): IO[Instrs] =
    comparison(floating, !unsigned, args, target)(InstrCond.GEq, InstrCond.NBelow)

  def abs(floating: Boolean)(args: Seq[Value], target: Register): IO[Instrs] = {
    verify(!floating, "abs operation", "floating point operations are not yet supported") *> IO {
      val arg = args.head
      val s1 = arg moveTo target

      val s2 = Instrs( // Prepare 'y', which is the sign of x extended to its full width
        MOV(arg._1, MovInterm, arg._2.operandSize),
        SAR((arg._2.bytes - 1).uconst, MovInterm, arg._2.operandSize)
      )

      val s3 = Instrs( // Do the actual ABS operation, (x XOR y) - y
        XOR(MovInterm, target, arg._2.operandSize),
        SUB(MovInterm, target, arg._2.operandSize)
      )

      s1 ++ s2 ++ s3
    }
  }

  def neg(floating: Boolean)(args: Seq[Value], target: Register): IO[Instrs] = {
    verify(!floating, "neg operation", "floating point operations are not yet supported") *> IO {
      args.head.moveTo(target) + NEG(target, args.head._2.operandSize)
    }
  }

  def sign(floating: Boolean)(args: Seq[Value], target: Register): IO[Instrs] = {
    verify(!floating, "sign operation", "floating point operations are not yet supported") *> IO {
      val (arg, _) = args.head
      Instrs(
        CMP(0.const, arg),
        SET(InstrCond.Neg, MovInterm),
        SET(InstrCond.NZero, target),
        NEG(MovInterm),
        OR(MovInterm, target)
      )
    }
  }

  def round(args: Seq[Value], target: Register, ctx: Context): IO[Instrs] =
    raise("round operation", "floating point operations are not yet supported")

  def trunc(args: Seq[Value], target: Register, ctx: Context): IO[Instrs] =
    raise("trunc operation", "floating point operations are not yet supported")

  def resizeWord(tt: DataType)(args: Seq[Value], target: Register): IO[Instrs] =
    IO(args.head.moveTo(value(target, tt)))

  def resizeFlot(tt: DataType)(args: Seq[Value], target: Register, ctx: Context): IO[Instrs] =
    raise("resize float operation", "floating point operations are not yet supported")

  def castWord(args: Seq[Value], target: Register, ctx: Context): IO[Instrs] =
    raise("cast to word operation", "floating point operations are not yet supported")

  def castFlot(args: Seq[Value], target: Register, ctx: Context): IO[Instrs] =
    raise("cast to float operation", "floating point operations are not yet supported")

}

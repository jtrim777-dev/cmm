package dev.jtrim777.cmm
package compile.x86_64

import compile.{CompilationContext, CompilePhase, CompiledProgram, genID, raise, verify}
import isa.{ISeq, Label}
import isa.x86_64.{ArchX64, InstrCond}
import isa.x86_64.X64Instr._
import isa.x86_64.dsl._
import isa.x86_64.X64.argType._
import isa.x86_64.registers._
import lang.{DataType, Expression, Primitive, Program, Statement}

import cats.effect.IO
import cats.effect.IO.{defer, pure}
import cats.effect.syntax._
import cats.implicits._
import dev.jtrim777.cmm.lang.ProgramSegment.ProcDefn

object CompileX64 extends CompilePhase[ArchX64]("compileX64") {
  override def body: Phase[IO, Program, CompiledProgram[ArchX64]] = ???

  type Context = CompilationContext[ArchX64]

  def compileExpression(expr: Expression, ctx: Context, target: Register = RAX): IO[Instrs] = expr match {
    case Expression.CInt(value) => MOV(value.const, target).wrap.pure
    case Expression.CFlot(_) => raise("float literal", "Floats are not yet supported")
    case Expression.ID(name) => IO(ctx.scope(name) match {
      case CompilationContext.ProcParam(_, index, kind) => value(fxArg(index), kind).moveTo(target)
      case CompilationContext.LocalVar(kind, pos) => value(pos, kind).moveTo(target)
      case CompilationContext.Procedure(procID) => LEAQ(LabelRef.proc(procID), target).wrap
      case CompilationContext.DataLabel(_, _, ref) => LEAQ(ref, target).wrap
    })
    case Expression.Read(kind, pos, align) =>
      if (align.isDefined) raise("read expr.", "Read alignment is not yet supported") else {
        compileExpression(pos, ctx, target).map { rez =>
          rez + MOV(Offset(target, 0.const), target, kind.operandSize)
        }
      }

    case Expression.Operation(op, args) =>
      /*
      val (lhv, lt) = helpers.argFromImmediate(lhs, ctx)
      val (rhv, rt) = helpers.argFromImmediate(rhs, ctx)
      val selTyp = lt.bounding(rt).get

      val normPrepare = helpers.typedMov((lhv, lt), (target, selTyp), !op.unsigned) ++
        helpers.typedMov((rhv, rt), (MovInterm, selTyp), !op.unsigned)

      val toRep = (Set(RAX, RDX) - target).toSeq
      val extPrepare = toRep.map(PUSH.apply(_)) ++
        helpers.typedMov((lhv, lt), (RAX, selTyp), !op.unsigned) ++
        (if (rhv.isInstanceOf[Offset]) {
          helpers.typedMov((rhv, rt), (MovInterm, selTyp), !op.unsigned)
        } else Seq.empty)
      val extRestore = toRep.map(POP.apply(_))
      val extRight: ISArg.RMArg = rhv match {
        case ra:ISArg.RMArg => ra
        case _ => MovInterm
      }

      op match {
        case _:ArithOp.Add => normPrepare :+ ADD(target, MovInterm)
        case _:ArithOp.Sub => normPrepare :+ SUB(target, MovInterm)
        case m:ArithOp.Mul if m.unsigned =>
          val i1 = MUL(extRight, selTyp.asSize)

          ???
        case _:ArithOp.Mul => ???
        case d:ArithOp.Div if d.unsigned => ???
        case _:ArithOp.Div => ???
        case m:ArithOp.Mod if m.unsigned => ???
        case _:ArithOp.Mod => ???
        case ArithOp.And => ???
        case ArithOp.Or => ???
        case ArithOp.Xor => ???
        case ArithOp.ShiftL => ???
        case ArithOp.ShiftR(signed) => ???
      }
      */
      ???
  }

  def compileOperation(op: Primitive, args: Seq[Value]): IO[Instrs] = ???

  def getType(expr: Expression, ctx: Context): IO[DataType] = expr match {
    case Expression.CInt(_) => DataType.Word8.pure[IO]
    case Expression.CFlot(_) => DataType.Flot8.pure[IO]
    case Expression.ID(name) => ctx.scope(name) match {
      case CompilationContext.ProcParam(_, _, kind) => kind.pure
      case CompilationContext.LocalVar(kind, _) => kind.pure
      case CompilationContext.Procedure(_) => DataType.Word8.pure[IO]
      case CompilationContext.DataLabel(kind, _, _) => kind.pure
    }
    case Expression.Read(kind, _, _) => kind.pure
    case Expression.Operation(op, args) => args.map(getType(_, ctx)).sequence.flatMap { argTypes =>
      op.getOutput(argTypes) match {
        case Right(value) => pure(value)
        case Left(err) => raise(s"data type of ${expr.toCode}", err)
      }
    }
  }

  //  def compileCondJump(lt: DataType, rt: DataType, op: Primitive, target: LabelRef): IO[Instrs] = {
  //    if (lt.superName != rt.superName) {
  //      raise("cond. jump", s"$lt and $rt cannot be compared")
  //    } else if (lt.superName == "float" || op.floating) {
  //      raise("cond. jump", "Floating point ops are not yet supported")
  //    } else {
  //      val jumpCond: InstrCond = op match {
  //        case Primitive.Eq => InstrCond.Eq
  //        case Primitive.NEq => InstrCond.NEq
  //        case Primitive.ULT => InstrCond.Below
  //        case Primitive.LT => InstrCond.Lt
  //        case Primitive.ULTE => InstrCond.NAbove
  //        case Primitive.LTE => InstrCond.LEq
  //        case Primitive.UGT => InstrCond.Above
  //        case Primitive.GT => InstrCond.Gt
  //        case Primitive.UGTE => InstrCond.NBelow
  //        case Primitive.GTE => InstrCond.GEq
  //        case _ => InstrCond.Eq
  //      }
  //
  //      val prepAndFin: IO[(Instrs, DataType)] = if (lt == rt) {
  //        (ISeq[ArchX64](), lt).pure
  //      } else if (lt.bytes < rt.bytes) {
  //        IO((value(RAX, lt).moveTo(value(RAX, rt), !op.unsigned), rt))
  //      } else {
  //        IO((value(SecTarget, rt).moveTo(value(SecTarget, lt), !op.unsigned), lt))
  //      }
  //
  //      for {
  //        pfr <- prepAndFin
  //        (prefix, finTyp) = pfr
  //        exec <- if (Primitive.Comparators.contains(op)) {
  //          CMP(SecTarget, RAX, finTyp.operandSize).wrap.pure[IO]
  //        } else {
  //          compileOperation(op, Seq(value(RAX, finTyp), value(SecTarget, finTyp)))
  //        }
  //      } yield prefix ++ exec + J(jumpCond, target)
  //    }
  //  }

  def conditionTypeForExpr(expr: Expression): InstrCond = expr match {
    case Expression.Operation(op, _) => op match {
      case Primitive.Eq => InstrCond.Eq
      case Primitive.NEq => InstrCond.NEq
      case Primitive.ULT => InstrCond.Below
      case Primitive.LT => InstrCond.Lt
      case Primitive.ULTE => InstrCond.NAbove
      case Primitive.LTE => InstrCond.LEq
      case Primitive.UGT => InstrCond.Above
      case Primitive.GT => InstrCond.Gt
      case Primitive.UGTE => InstrCond.NBelow
      case Primitive.GTE => InstrCond.GEq
      case _ => InstrCond.Eq
    }
    case _ => InstrCond.Eq
  }

  /**
   *
   * @param args
   * @param ctx
   * @return (Prelude instructions, cleanup instructions, updated context)
   */
  def compileJumpShuffle(args: Seq[Expression], ctx: Context): IO[(Instrs, Instrs, Context)] = {
    val needsShuffle = args.length != ctx.procArity && (args.length > 6 || ctx.procArity > 6)

    val savedParams = args.flatMap(helpers.findParameters(_, ctx))
    val pushParams = savedParams.flatMap(p => safePush(helpers.argPosToVR(p.index).asArg))
    val updatedCtx = savedParams.foldLeft(ctx) { (c, p) =>
      c.addLocal(p.name, p.kind, 8)
    }

    val argStart = Math.max((ctx.procArity - 5) * 8, 8)

    val saveCore = if (needsShuffle) Seq(
      MOV(Offset(RBP, 0.const), SecTarget),
      MOV(Offset(RBP, 8.const), RBX)
    ) else Seq.empty

    val updateArgs = args.zipWithIndex.flatMap { case (arg, i) =>
      val ca = compileExpression(arg, updatedCtx)
      val at = getType(arg, ctx)

      val copy = if (i < 6) {
        movTo(helpers.argPosToVR(i), RAX, at)
      } else {
        val offset = ((i - 6) * -8) + argStart
        saveMov(RAX, Offset(RBP, offset.const), at)
      }

      ca ++ copy
    }

    val reset = if (needsShuffle) {
      val raPos = argStart - Math.max((args.length - 6) * 8, 0)
      Seq(
        ADD((raPos - 8).const, RBP),
        MOV(RBX, Offset(RBP, 8.const)),
        MOV(SecTarget, Offset(RBP, 0.const)),
      )
    } else Seq.empty

    val clearStack = Seq(MOV(RBP, RSP))

    (pushParams ++ saveCore ++ updateArgs, reset ++ clearStack, updatedCtx)
  }

  def compileProcedureID(expr: Expression, ctx: Context): IO[(Instrs, Callable)] = {
    expr match {
      case Expression.ID(name) => ctx.scope.get(name) match {
        case Some(CompilationContext.Procedure(_)) => (ISeq.empty[ArchX64], LabelRef.proc(name)).pure[IO]
        case _ => compileExpression(expr, ctx).map(is => (is, RAX))
      }
      case _ => compileExpression(expr, ctx).map(is => (is, RAX))
    }
  }

  def compileStatement(stmt: Statement, ctx: Context, bid: String): IO[(Instrs, Context)] = stmt match {
    case Statement.Skip => (NOP.wrap, ctx).pure[IO]
    case Statement.VarDecl(kind, names@_*) =>
      IO(names.foldLeft(ctx) { (c, n) => c.addLocal(n, kind, 8) }).map { nc =>
        (ISeq.empty, nc)
      }
    case Statement.Assn(name, avalue) =>
      for {
        cellOpt <- IO(ctx.scope.get(name))
        cell <- cellOpt match {
          case None => raise("assignment", s"Cannot assign to undeclared variable $name")
          case Some(v: CompilationContext.LocalVar[ArchX64]) => v.pure[IO]
          case Some(_) => raise("assignment", s"Cannot assign to name $name which is not a local var")
        }
        et <- getType(avalue, ctx)
        _ <- verify(et == cell.kind, "assignment", s"Cannot assign value of type $et to name $name which is of type ${cell.kind}")
        ce <- compileExpression(avalue, ctx)
        fin = value(cell.pos, et) moveTo RAX
      } yield (ce ++ fin, ctx)
    case Statement.Write(kind, pos, avalue, _) =>
      for {
        pt <- getType(pos, ctx)
        _ <- verify(pt.canWidenTo(DataType.Word8), "write", s"$pt is not a valid type for write position")
        et <- getType(avalue, ctx)
        _ <- verify(et.canWidenTo(kind), "write", s"Cannot write $et as $kind")
        cp <- compileExpression(pos, ctx, target = SecTarget)
        cv <- compileExpression(avalue, ctx)
        widener = if (pt != DataType.Word8) {
          value(SecTarget, pt) moveTo value(SecTarget, DataType.Word8)
        } else ISeq.empty[ArchX64]
        finOp = value(RAX, et) moveTo value(Offset(SecTarget, 0.uconst), kind)
        proc = cp ++ cv ++ widener ++ finOp
      } yield (proc, ctx)
    case Statement.IfStmt(cond, exec, elseExec) =>
      for {
        stmtID <- genID()
        target = elseExec
          .map(_ => LabelRef.elseCase(stmtID))
          .getOrElse(LabelRef.endIf(stmtID))
        finJump = elseExec
          .map(_ => JMP(LabelRef.endIf(stmtID)).wrap)
          .getOrElse(ISeq.empty[ArchX64])
        condC <- compileExpression(cond, ctx)
        jtyp = conditionTypeForExpr(cond).negate
        jumpC = J(jtyp, target).wrap
        execC <- compileBlock(exec, ctx)
        elseC <- elseExec match {
          case Some(elseBody) => compileBlock(elseBody, ctx).map(_.label(Label.elseCase(stmtID)))
          case None => ISeq.empty[ArchX64].pure[IO]
        }
        proc = (condC ++ jumpC ++ execC ++ finJump ++ elseC).labelEnd(Label.endIf(stmtID))
      } yield (proc, ctx)
    case Statement.LocalLabel(name) => pure((ISeq.empty[ArchX64].label(Label.localMark(bid, name)), ctx))
    case Statement.Goto(label) => (JMP(LabelRef.localMark(bid, label)).wrap, ctx).pure
    case Statement.Jump(proc, args@_*) =>
      for {
        cjs <- compileJumpShuffle(args, ctx)
        (prelude, clearup, modCtx) = cjs
        cpi <- compileProcedureID(proc, modCtx)
        (tget, target) = cpi
        proci = (prelude ++ tget ++ clearup) + JMP(target)
      } yield (proci, ctx)
    case Statement.Call(results, proc, args@_*) =>
      for {
        resultPositions <- results.map { s =>
          ctx.scope.get(s) match {
            case Some(v: CompilationContext.LocalVar[ArchX64]) => IO.pure(v)
            case Some(o) => raise("call", s"Invalid variable $o for return value")
            case None => raise("call", s"Cannot assign result to undeclared variable $s")
          }
        }.sequence

        saveArgs <- IO(ISeq.flatten(args.indices.take(6).map(i => fxArg(i).resolve.push)))

        first6 = args.take(6)
        remArgs = args.drop(6).reverse
        setArgs <- (first6 ++ remArgs).zipWithIndex.map { case (arg, i) =>
          for {
            at <- getType(arg, ctx)
            ca <- compileExpression(arg, ctx)
            copy = if (i < 6) {
              MOV(RAX, fxArg(i).resolve, at.operandSize)
            } else PUSH(RAX, at.operandSize)
          } yield ca + copy
        }.sequence.map(ISeq.flatten)

        cpi <- compileProcedureID(proc, ctx)
        (tget, target) = cpi

        callIt = CALL(target).wrap

        restore1 <- IO(if (args.length > 6) ADD(((args.length - 6) * 8).uconst, RSP).wrap else ISeq.empty[ArchX64])
        restore2 <- IO(args.indices.take(6).reverse.map { i =>
          POP(fxArg(i).resolve.asInstanceOf[Register]).wrap
        }).map(ISeq.flatten)
        restore = restore1 ++ restore2

        copyResults <- IO(resultPositions.zip(Seq(RAX, RBX)).map { case (dst, src) =>
          value(src, dst.kind) moveTo dst.pos
        }).map(ISeq.flatten)

        prog = saveArgs ++ setArgs ++ tget ++ callIt ++ restore ++ copyResults
      } yield (prog, ctx)
    case Statement.Return(results@_*) =>
      results.zip(Seq(RAX, RBX)).map { case (expr, dst) =>
        compileExpression(expr, ctx, dst)
      }.sequence.map(ISeq.flatten).map((_, ctx))
    case b: Statement.Block => compileBlock(b, ctx).map((_, ctx))
  }

  def compileBlock(block: Statement.Block, ctx: Context): IO[Instrs] = {
    val bc = IO.pure((ISeq.empty[ArchX64], ctx))

    genID().flatMap { blockID =>
      block.stmts.foldLeft(bc) { case (acc, stmt) =>
        acc.flatMap { case (rez, context) =>
          compileStatement(stmt, context, blockID).map { case (nr, nc) => (rez ++ nr, nc) }
        }
      }
    }.map(_._1)
  }

  def compileProcedure(defn: ProcDefn, context: Context): IO[Instrs] = {
    for {
      depth <- IO(defn.wordUsage(8))
      truDepth = if (depth % 2 == 1) depth + 1 else depth

      prologue = ISeq.labeled[ArchX64](Label.proc(defn.name), PUSH(RBP))
        .add(MOV(RSP, RBP))
        .addLabeled(SUB(UConstant(truDepth * 8), RSP), Label.procBody(defn.name))

      entryContext <- IO(defn.params.zipWithIndex.foldLeft(context) { case (ctx, ((name, dt), ind)) =>
        ctx.enscope(name, CompilationContext.ProcParam(name, ind, dt))
      })

      body <- compileBlock(defn.body, entryContext.setArity(defn.params.length))

      epilogue = ISeq.apply[ArchX64](
        MOV(RBP, RSP),
        POP(RBP),
        RET()
      )
    } yield prologue ++ body ++ epilogue
  }
}
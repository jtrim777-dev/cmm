package dev.jtrim777.cmm
package x86_64

import java.util.UUID

import asm.{AsmSym, ISArg}
import asm.AsmSym.Label
import asm.ISArg._
import dev.jtrim777.cmm.lang.Statement.Jump
import common.Context
import lang.ProgramSegment.ProcDefn
import lang.{ArithOp, DataType, Expression, OpFlag, RelOp, Statement}
import x86_64.ISAx86_64._
import x86_64.registers._
import helpers.{DTHelper, LongNumSyntax, NumSyntax, VRHelper, movFrom, movTo, safePush, saveMov}

object compile {
  def newID(): String = UUID.randomUUID().toString.replace('-', '_')

  def raise(region: String, problem: String): Nothing =
    throw new IllegalArgumentException(s"Error compiling $region: $problem")

  def compileExpression(expr: Expression, ctx: Context, target: Register = RAX): Output = expr match {
    case Expression.CInt(value) => Seq(MOV(value.const, target))
    case Expression.CFlot(_) => raise("float literal", "Floats are not yet supported")
    case Expression.ID(name) => ctx.scope(name) match {
      case Context.ProcParam(_, index, kind) => movFrom(helpers.argPosToVR(index), target, kind)
      case Context.LocalVar(kind, pos) => movFrom(pos, target, kind)
      case Context.Procedure(procID) => Seq(LEAQ(LabelRef.proc(procID), target))
      case Context.DataLabel(_, _, ref) => Seq(LEAQ(ref, target))
    }
    case Expression.Read(kind, pos, align) =>
      if (align.isDefined) raise("read expr.", "Read alignment is not yet supported")

      val getPos = compileExpression(pos, ctx, target)
      getPos :+ MOV(Offset(target, 0.const), target, kind.asSize)
    case Expression.InfixOp(lhs, op, rhs) =>
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
    case Expression.PrefixOp(op, target) => ???
    case Expression.PrimOp(op, args) => ???
  }

  def getType(expr: Expression, ctx: Context): DataType = expr match {
    case Expression.CInt(_) => DataType.Word8
    case Expression.CFlot(_) => DataType.Flot8
    case Expression.ID(name) => ctx.scope(name) match {
      case Context.ProcParam(_, _, kind) => kind
      case Context.LocalVar(kind, _) => kind
      case Context.Procedure(_) => DataType.Word8
      case Context.DataLabel(kind, _, _) => kind
    }
    case Expression.Read(kind, _, _) => kind
    case Expression.InfixOp(lhs, op, rhs) =>
      val lt = getType(lhs, ctx)
      val rt = getType(rhs, ctx)
      val out = lt.bounding(rt)

      if (op.flags.contains(OpFlag.Floating)) {
        val size = Seq(lt, rt).maxBy(_.bytes).bytes
        if (size < 8) DataType.Flot4 else DataType.Flot8
      } else {
        out.getOrElse(DataType.Flot8)
      }
    case Expression.PrefixOp(_, target) => getType(target, ctx)
    case Expression.PrimOp(op, args) => op.output(args.map(getType(_, ctx)))
  }

  def compileCondJump(lt: DataType, rt: DataType, op: RelOp, target: LabelRef): Output = {
    if (lt.superName != rt.superName) {
      raise("cond. jump", s"$lt and $rt cannot be compared")
    } else if (lt.superName == "float") {
      raise("cond. jump", "Floating point ops are not yet supported")
    }

    val jumpCond: ISAx86_64.Cond = op match {
      case _: RelOp.EQ => Eq
      case _: RelOp.NE => NEq
      case o: RelOp.LT if o.flags.contains(OpFlag.Unsigned) => Below
      case _: RelOp.LT => Lt
      case o: RelOp.LE if o.flags.contains(OpFlag.Unsigned) => NAbove
      case _: RelOp.LE => LEq
      case o: RelOp.GT if o.flags.contains(OpFlag.Unsigned) => Above
      case _: RelOp.GT => Gt
      case o: RelOp.GE if o.flags.contains(OpFlag.Unsigned) => NBelow
      case _: RelOp.GE => GEq
    }

    val (extension, finTyp) = if (lt == rt) {
      (Seq.empty, lt)
    } else if (lt.bytes < rt.bytes) {
      (helpers.typedMov((RAX, lt), (RAX, rt), !op.flags.contains(OpFlag.Unsigned)), rt)
    } else {
      (helpers.typedMov((SecTarget, rt), (SecTarget, lt), !op.flags.contains(OpFlag.Unsigned)), lt)
    }

    extension ++ Seq(CMP(SecTarget, RAX, finTyp.asSize), J(jumpCond, target))
  }

  def compileJumpShuffle(args: Seq[Expression], ctx: Context): (Output, Output, Context) = {
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

  def compileProcedureID(expr: Expression, ctx: Context): (Output, Callable) = {
    expr match {
      case Expression.ID(name) => ctx.scope.get(name) match {
        case Some(Context.Procedure(_)) => (Seq.empty, LabelRef.proc(name))
        case _ => (compileExpression(expr, ctx), RAX)
      }
      case _ => (compileExpression(expr, ctx), RAX)
    }
  }

  def compileStatement(stmt: Statement, ctx: Context, bid: String): (Output, Context) = stmt match {
    case Statement.Skip => (Seq(NOP), ctx)
    case Statement.VarDecl(kind, names@_*) =>
      val nc = names.foldLeft(ctx) { (c, n) => c.addLocal(n, kind, 8) }
      (Seq.empty, nc)
    case Statement.Assn(name, value) =>
      val cell = ctx.scope.getOrElse(name, raise("assignment", s"Cannot assign to undeclared variable $name")) match {
        case v: Context.LocalVar => v
        case _ => raise("assignment", s"Cannot assign to name $name which is not a local var")
      }

      val et = getType(value, ctx)

      if (et != cell.kind) {
        raise("assignment", s"Cannot assign value of type $et to name $name which is of type ${cell.kind}")
      }

      val ce = compileExpression(value, ctx)

      (ce ++ helpers.movTo(cell.pos, RAX, et), ctx)
    case Statement.Write(kind, pos, value, _) =>
      val pt = getType(pos, ctx)
      if (!pt.canWidenTo(DataType.Word8)) {
        raise("write", s"$pt is not a valid type for write position")
      }

      val et = getType(value, ctx)
      if (!et.canWidenTo(kind)) {
        raise("write", s"Cannot write $et as $kind")
      }

      val cp = compileExpression(pos, ctx, target = SecTarget)
      val cv = compileExpression(value, ctx)

      val widener = if (pt != DataType.Word8) {
        helpers.typedMov((SecTarget, pt), (SecTarget, DataType.Word8))
      } else Seq.empty

      val proc = cp ++ cv ++ widener ++ helpers.typedMov((RAX, et), (Offset(SecTarget, UConstant(0)), kind))
      (proc, ctx)
    case Statement.IfStmt(lhs, op, rhs, exec, elseExec) =>
      val lt = getType(lhs, ctx)
      val rt = getType(rhs, ctx)

      val stmtID = newID()

      val target = elseExec
        .map(_ => LabelRef.elseCase(stmtID))
        .getOrElse(LabelRef.endIf(stmtID))

      val finJump = elseExec
        .map(_ => Seq(JMP(LabelRef.endIf(stmtID))))
        .getOrElse(Seq.empty)

      val proc = compileExpression(lhs, ctx) ++
        compileExpression(rhs, ctx, target = SecTarget) ++
        compileCondJump(lt, rt, op.negate, target) ++
        compileBlock(exec, ctx) ++
        finJump ++
        elseExec.map(b => Label.elseCase(stmtID) +: compileBlock(b, ctx)).getOrElse(Seq.empty) ++
        Seq(Label.endIf(stmtID))

      (proc, ctx)
    case Statement.LocalLabel(name) => (Seq(Label.localMark(bid, name)), ctx)
    case Statement.Goto(label) =>
      (Seq(JMP(LabelRef.localMark(bid, label))), ctx)
    case Statement.Jump(proc, args@_*) =>
      val (prelude, clearup, modCtx) = compileJumpShuffle(args, ctx)

      val (tget, target: Callable) = compileProcedureID(proc, modCtx)

      val proci = prelude ++ tget ++ clearup :+ JMP(target)
      (proci, ctx)
    case Statement.Call(results, proc, args@_*) =>
      val resultPositions = results.map(ctx.scope.apply).map {
        case v:Context.LocalVar => v
        case o => raise("call", s"Invalid variable $o for return value")
      }

      val saveArgs = args.indices.take(6).map(i => PUSH(helpers.argPosToVR(i).asArg))

      val first6 = args.take(6)
      val remArgs = args.drop(6).reverse
      val setArgs = (first6 ++ remArgs).zipWithIndex.flatMap { case (arg, i) =>
        val at = getType(arg, ctx)
        val ca = compileExpression(arg, ctx)
        val copy = if (i < 6) {
          MOV(RAX, helpers.argPosToVR(i).asArg, at.asSize)
        } else PUSH(RAX, at.asSize)

        ca :+ copy
      }

      val (tget, target: Callable) = compileProcedureID(proc, ctx)

      val callIt = Seq(CALL(target))

      val restore = {
        val p1 = if (args.length > 6) Seq(ADD(((args.length-6)*8).uconst, RSP)) else Seq.empty
        val p2 = args.indices.take(6).reverse.map { i =>
          POP(helpers.argPosToVR(i).asArg.asInstanceOf[Register])
        }

        p1 ++ p2
      }

      val copyResults = resultPositions.zip(Seq(RAX, RBX)).flatMap { case (dst, src) =>
        helpers.movTo(dst.pos, src, dst.kind)
      }

      val prog = saveArgs ++ setArgs ++ tget ++ callIt ++ restore ++ copyResults
      (prog, ctx)
    case Statement.Return(results@_*) =>
      (results.zip(Seq(RAX, RBX)).flatMap { case (expr, dst) =>
        compileExpression(expr, ctx, dst)
      }, ctx)
    case b:Statement.Block => (compileBlock(b, ctx), ctx)
  }

  def compileBlock(block: Statement.Block, ctx: Context): Output = {
    val id = newID()

    block.stmts.foldLeft((Seq.empty[AsmSym], ctx)) { case ((rez, context), stmt) =>
      val (nr, nc) = compileStatement(stmt, context, id)
      (rez ++ nr, nc)
    }._1
  }

  def compileProcedure(defn: ProcDefn, context: Context): Output = {
    val depth = defn.wordUsage(8)
    val truDepth = if (depth % 2 == 1) depth + 1 else depth

    val prologue: Output = Seq(
      Label.proc(defn.name),
      PUSH(RBP),
      MOV(RSP, RBP),
      Label.procBody(defn.name),
      SUB(UConstant(truDepth * 8), RSP)
    )

    val entryContext = defn.params.zipWithIndex.foldLeft(context) { case (ctx, ((name, dt), ind)) =>
      ctx.enscope(name, Context.ProcParam(name, ind, dt))
    }

    val body = compileBlock(defn.body, entryContext.setArity(defn.params.length))

    val epilogue = Seq(
      MOV(RBP, RSP),
      POP(RBP),
      RET()
    )

    prologue ++ body ++ epilogue
  }


}

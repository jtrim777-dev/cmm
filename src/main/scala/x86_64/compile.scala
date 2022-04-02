package com.github.jtrim777.cmm
package x86_64

import asm.AsmSym
import asm.AsmSym.Label
import asm.ISArg._
import common.Context
import lang.ProgramSegment.ProcDefn
import lang.{DataType, Expression, Statement}
import x86_64.ISAx86_64._
import x86_64.registers._

object compile {
  def raise(region: String, problem: String): Nothing =
    throw new IllegalArgumentException(s"Error compiling $region: $problem")

  def compileExpression(expr: Expression, ctx: Context, target: Register = RAX): Output = ???

  def getType(expr: Expression, ctx: Context): DataType = ???

  def compileStatement(stmt: Statement, ctx: Context): (Output, Context) = stmt match {
    case Statement.Skip => (Seq(NOP), ctx)
    case Statement.VarDecl(kind, names@_*) =>
      val nc = names.foldLeft(ctx) { (c, n) => c.addLocal(n, kind, 8) }
      (Seq.empty, nc)
    case Statement.Assn(name, value) =>
      val cell = ctx.scope.data.getOrElse(name, raise("assignment", s"Cannot assign to undeclared variable $name")) match {
        case v:Context.LocalVar => v
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

      val cp = compileExpression(pos, ctx, target = Scratch2)
      val cv = compileExpression(value, ctx)

      val widener = if (pt != DataType.Word8) {
        helpers.typedMov((Scratch2, pt), (Scratch2, DataType.Word8), sign = false)
      } else Seq.empty

      val proc = cp ++ cv ++ widener ++ helpers.typedMov((RAX, et), (Offset(Scratch2, UConstant(0)), kind))
      (proc, ctx)
    case Statement.IfStmt(lhs, op, rhs, exec, elseExec) =>
      val lt = getType(lhs, ctx)
      val rt = getType(rhs, ctx)

      if (lt.superName != rt.superName) {
        raise("if statements", s"$lt and $rt cannot be compared")
      }


    case Statement.LocalLabel(name) =>
    case Statement.Goto(label) =>
    case Statement.Jump(proc, args@_*) =>
    case Statement.Call(results, proc, args@_*) =>
    case Statement.Return(results@_*) =>
    case Statement.Block(stmts@_*) =>
  }

  def compileBlock(block: Statement.Block, ctx: Context): (Output, Context) = {
    block.stmts.foldLeft((Seq.empty[AsmSym], ctx)) { case ((rez, context), stmt) =>
      val (nr, nc) = compileStatement(stmt, context)
      (rez ++ nr, nc)
    }
  }

  def compileProcedure(defn: ProcDefn, context: Context): Output = {
    val depth = defn.wordUsage(8)
    val truDepth = if (depth % 2 == 1) depth + 1 else depth

    val prologue: Output = Seq(
      Label.proc(defn.name),
      PUSH(RBP),
      MOV(RBP, RSP),
      Label.procBody(defn.name),
      SUB(UConstant(truDepth * 8), RSP)
    )

    val entryContext = defn.params.zipWithIndex.foldLeft(context) { case (ctx, ((name, dt), ind)) =>
      val pos = helpers.argPosToVR(ind)
      ctx.enscopeLocal(name, dt, pos)
    }

    val (body, _) = compileBlock(defn.body, entryContext)

    val epilogue = Seq(
      MOV(RSP, RBP),
      POP(RBP),
      RET()
    )

    prologue ++ body ++ epilogue
  }


}

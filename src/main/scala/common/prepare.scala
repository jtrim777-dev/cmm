package com.github.jtrim777.cmm
package common

import lang.Expression._
import lang.ProgramSegment.{DataBlock, ProcDefn}
import lang.Statement._
import lang._

object prepare {

  def prepare(prog: Program): Program = {
    val procs = prog.procedures
    val dbs = prog.dataBlocks.flatMap(_.decls)

    val (init, db) = generateInit(dbs)

    val nprocs = procs.map(d => d.copy(body = normalize(d.body).asInstanceOf[Block]))

    Program((prog.ports ++ Seq(db, init) ++ nprocs):_*)
  }

  def generateInit(dbs: Seq[DataDecl]): (ProcDefn, DataBlock) = {
    val stmts = dbs.flatMap(generateInit)

    val defn = ProcDefn("cmm$initData", Seq.empty, Statement.Block(stmts.map(normalize):_*))

    val ndecls = dbs.map { decl =>
      DataDecl(decl.name, decl.kind, decl.realCount, Seq.empty)
    }

    (defn, DataBlock(ndecls:_*))
  }

  def generateInit(decl: DataDecl): Seq[Statement] = {
    val rc = decl.realCount

    if (rc == 0 || decl.value.isEmpty) {
      Seq.empty
    } else if (rc == decl.value.length) {
      decl.value.zipWithIndex.map { case (expr, i) =>
        Write(decl.kind, InfixOp(ID(decl.name), ArithOp.Add(), CInt(i)), expr, None)
      }
    } else {
      val size = decl.count
      val base = decl.value.length

      (0 until size).map { i =>
        val expr = decl.value(i % base)
        Write(decl.kind, InfixOp(ID(decl.name), ArithOp.Add(), CInt(i)), expr, None)
      }
    }
  }

  def isImmediate(expr: Expression): Boolean = expr match {
    case CInt(_) => true
    case CFlot(_) => true
    case ID(_) => true
    case _ => false
  }

  def isMemable(expr: Expression): Boolean = expr match {
    case InfixOp(lhs, op, rhs) => op match {
      case ArithOp.Add(flags@_*) if flags.isEmpty => isImmediate(lhs) && isImmediate(rhs)
      case ArithOp.Sub(flags@_*) if flags.isEmpty => isImmediate(lhs) && isImmediate(rhs)
      case _ => false
    }
    case _ => isImmediate(expr)
  }

  def isNormal(expr: Expression): Boolean = expr match {
    case Read(_, pos, _) => isMemable(pos)
    case InfixOp(lhs, _, rhs) => isImmediate(lhs) && isImmediate(rhs)
    case PrefixOp(_, target) => isImmediate(target)
    case PrimOp(_, args) => args.forall(isImmediate)
    case o => isImmediate(o)
  }

  def normalize(expr: Expression, l: String): (Seq[Statement], Expression) = expr match {
    case Read(kind, pos, align) =>
      val (pw, np) = normalize(pos, l + "pos")
      (pw :+ Assn(l, Read(kind, np, align)), ID(l))
    case InfixOp(lhs, op, rhs) =>
      val (lw, lp) = if (isNormal(lhs)) (Seq.empty, lhs) else normalize(lhs, l + "lhs")
      val (rw, rp) = normalize(rhs, l + "rhs")
      (lw ++ rw :+ Assn(l, InfixOp(lp, op, rp)), ID(l))
    case PrefixOp(op, target) =>
      val (tw, nt) = normalize(target, l + "tgt")
      (tw :+ Assn(l, PrefixOp(op, nt)), ID(l))
    case PrimOp(op, args) =>
      val (aw, nas) = normMany(args, l + "pg")
      (aw :+ Assn(l, PrimOp(op, nas)), ID(l))
    case _ => (Seq.empty, expr)
  }

  def normMany(exprs: Seq[Expression], l: String): (Seq[Statement], Seq[Expression]) = {
    val base = (Seq.empty[Statement], Seq.empty[Expression])
    exprs.zipWithIndex.foldLeft(base) { case ((works, ngs), (ag, i)) =>
      if (isImmediate(ag)) {
        (works, ngs :+ ag)
      } else {
        val (work, na) = normalize(ag, "cmm$norm$"+l+i+"$")
        (works ++ work, ngs :+ na)
      }
    }
  }

  def normForCall(proc: Expression, args: Seq[Expression]): (Seq[Statement], Expression, Seq[Expression]) = {
    if(isImmediate(proc) && args.forall(isImmediate)) {
      (Seq.empty, proc, args)
    } else {
      val (procWork, np) = normalize(proc, "cmm$norm$tgt")
      val (argWork, nargs) = normMany(args, "arg")

      (procWork ++ argWork, np, nargs)
    }
  }

  def normalize(stmt: Statement): Statement = stmt match {
    case Assn(name, value) => if (isNormal(value)) stmt else {
      val (work, nn) = normalize(value, s"cmm$$norm$$$name$$")
      val sts = work map {
        case Assn(n, e) if n == nn.toCode => Assn(name, e)
        case o => o
      }
      Block(sts:_*)
    }
    case Write(kind, pos, value, align) => if (isMemable(pos) && isNormal(value)) stmt else {
      val (work1, npn) = normalize(pos, "cmm$norm$pos$")
      val (work2, nvn) = normalize(value, "cmm$norm$value$")
      Block((work1 ++ work2 :+ Write(kind, npn, nvn, align)):_*)
    }
    case IfStmt(lhs, op, rhs, exec, elseExec) => {
      if (isImmediate(lhs) && isImmediate(rhs)) {
        IfStmt(lhs, op, rhs, normalize(exec).asInstanceOf[Block],
          elseExec.map(e => normalize(e).asInstanceOf[Block]))
      } else {
        val (work1, nln) = normalize(lhs, "cmm$norm$lhs$")
        val (work2, nrn) = normalize(rhs, "cmm$norm$rhs$")
        Block((work1 ++ work2 :+ IfStmt(nln, op, nrn, normalize(exec).asInstanceOf[Block],
          elseExec.map(e => normalize(e).asInstanceOf[Block]))):_*)
      }
    }
    case Jump(proc, args@_*) =>
      val (work, np, nas) = normForCall(proc, args)
      if (work.isEmpty) stmt else {
        Block(work :+ Jump(np, nas:_*):_*)
      }
    case Call(results, proc, args@_*) =>
      val (work, np, nas) = normForCall(proc, args)
      if (work.isEmpty) stmt else {
        Block(work :+ Call(results, np, nas:_*):_*)
      }
    case Return(results@_*) => if (results.forall(isImmediate)) stmt else {
      val (argWork, nrez) = normMany(results, "rez")
      Block(argWork :+ Return(nrez:_*):_*)
    }
    case Block(stmts@_*) => Block(stmts.map(normalize):_*)
    case _ => stmt
  }
}

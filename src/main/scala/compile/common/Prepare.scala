package dev.jtrim777.cmm
package compile.common

import lang.Expression._
import lang.ProgramSegment.{DataBlock, ProcDefn}
import lang.Statement._
import lang._
import Phase.{Printer, phase}

import collection.immutable.Seq
import cats.effect.IO
import cats.implicits.toTraverseOps
import cats.effect.IO.pure
import dev.jtrim777.cmm.Phase

object Prepare extends Phase.Group[IO, Program, Program]("prepare") {
  override def body: Phase[IO, Program, Program] = {
    extractDataInit *> normalizeProcedures
  }

  // MARK: Begin sub-phases

  def extractDataInit: Phase[IO, Program, Program] = phase("extractDataInit"){ (prog, log) =>
    val dbs = prog.dataBlocks.flatMap(_.decls)

    val computeStmts = dbs.map(generateInit).sequence.map(_.flatten)

    computeStmts.map { stmts =>
      val dataInitProc = ProcDefn("cmm$initData", Seq.empty, stmts.asInstanceOf[Block])

      val ndecls = dbs.map { decl =>
        DataDecl(decl.name, decl.kind, decl.realCount, Seq.empty)
      }

      prog.copy(dataBlocks = Seq(DataBlock(ndecls)), procedures = dataInitProc +: prog.procedures)
    }
  }

  def normalizeProcedures: Phase[IO, Program, Program] = phase("normalizeProcedures"){ (prog, log) =>
    prog.procedures.map { proc =>
      normalize(proc.body).map { bsts => proc.copy(body = bsts.asInstanceOf[Block]) }
    }.sequence.map(nprocs => prog.copy(procedures = nprocs))
  }

  // MARK: Begin helpers

  def generateInit(decl: DataDecl): IO[Seq[Statement]] = {
    val rc = decl.realCount

    if (rc == 0 || decl.value.isEmpty) {
      IO.pure(Seq.empty)
    } else if (rc == decl.value.length) IO {
      decl.value.zipWithIndex.map { case (expr, i) =>
        Write(decl.kind, Operation(Primitive.Add , Seq(ID(decl.name), CInt(i))), expr, None)
      }
    } else IO {
      val size = decl.count
      val base = decl.value.length

      (0 until size).map { i =>
        val expr = decl.value(i % base)
        Write(decl.kind, Operation(Primitive.Add, Seq(ID(decl.name), CInt(i))), expr, None)
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
    case Operation(op, args) => op match {
      case Primitive.Add => isImmediate(args.head) && isImmediate(args(1))
      case Primitive.Sub => isImmediate(args.head) && isImmediate(args(1))
      case _ => false
    }
    case _ => isImmediate(expr)
  }

  def isNormal(expr: Expression): Boolean = expr match {
    case Read(_, pos, _) => isMemable(pos)
    case Operation(_, args) => args.forall(isImmediate)
    case o => isImmediate(o)
  }

  /**
   * Converts a possibly non-normal expression to a series of operations and the value to replace the expression with.
   * If the expression is already normal, the output will have no statements
   */
  def normalize(expr: Expression, l: String): IO[(Seq[Statement], Expression)] = expr match {
    case Read(kind, pos, align) =>
      normalize(pos, l + "pos").map { case (pw, np) =>
        (pw :+ Assn(l, Read(kind, np, align)), ID(l))
      }
    case Operation(op, args) =>
      normMany(args, l + "pg").map { case (aw, nas) =>
        (aw :+ Assn(l, Operation(op, nas)), ID(l))
      }
    case _ => pure((Seq.empty, expr))
  }

  def normMany(exprs: Seq[Expression], l: String): IO[(Seq[Statement], Seq[Expression])] = {
    val base = IO.pure((Seq.empty[Statement], Seq.empty[Expression]))
    exprs.zipWithIndex.foldLeft(base) { case (acc, (ag, i)) =>
      acc.flatMap { case (works, ngs) =>
        if (isImmediate(ag)) {
          pure((works, ngs :+ ag))
        } else {
          normalize(ag, "cmm$norm$" + l + i + "$").map { case (work, na) =>
            (works ++ work, ngs :+ na)
          }
        }
      }
    }
  }

  def normForCall(proc: Expression, args: Seq[Expression]): IO[(Seq[Statement], Expression, Seq[Expression])] = {
    if(isImmediate(proc) && args.forall(isImmediate)) {
      pure((Seq.empty, proc, args))
    } else {
      normalize(proc, "cmm$norm$tgt").flatMap { case (procWork, np) =>
        normMany(args, "arg").map { case (argWork, nargs) =>
          (procWork ++ argWork, np, nargs)
        }
      }
    }
  }

  def normalize(stmt: Statement): IO[Statement] = stmt match {
    case Assn(name, value) => if (isNormal(value)) pure(stmt) else {
      normalize(value, s"cmm$$norm$$$name$$").map { case (work, nn) =>
        val sts = work map {
          case Assn(n, e) if n == nn.toCode => Assn(name, e)
          case o => o
        }
        Block(sts: _*)
      }
    }
    case Write(kind, pos, value, align) => if (isMemable(pos) && isNormal(value)) pure(stmt) else {
      for {
        np <- normalize(pos, "cmm$norm$pos$")
        nv <- normalize(value, "cmm$norm$value$")
      } yield Block((np._1 ++ nv._1 :+ Write(kind, np._2, nv._2, align)):_*)
    }
    case IfStmt(cond, exec, elseExec) => {
      for {
        ncond <- if (isNormal(cond)) pure((Seq.empty, cond)) else normalize(cond, "cmm$norm$cond")
        nexecS <- normalize(exec)
        nexec = nexecS.asInstanceOf[Block]
        nelse <- elseExec match {
          case Some(elseB) => normalize(elseB).map(d => Some(d.asInstanceOf[Block]))
          case None => IO.pure(None)
        }
        out = IfStmt(ncond._2, nexec, nelse)
      } yield if (ncond._1.isEmpty) out else Block(ncond._1 :+ out)
    }
    case Jump(proc, args@_*) =>
      normForCall(proc, args).map { case (work, np, nas) =>
        if (work.isEmpty) stmt else {
          Block(work :+ Jump(np, nas: _*): _*)
        }
      }
    case Call(results, proc, args@_*) =>
      normForCall(proc, args).map { case (work, np, nas) =>
        if (work.isEmpty) stmt else {
          Block(work :+ Call(results, np, nas: _*): _*)
        }
      }
    case Return(results@_*) => if (results.forall(isImmediate)) pure(stmt) else {
      normMany(results, "rez").map { case (argWork, nrez) =>
        Block(argWork :+ Return(nrez: _*): _*)
      }
    }
    case Block(stmts@_*) => stmts.map(normalize).sequence.map(Block)
    case _ => pure(stmt)
  }
}

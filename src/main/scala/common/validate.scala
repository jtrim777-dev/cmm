package com.github.jtrim777.cmm
package common

import lang.ProgramSegment.{DataBlock, ProcDefn}
import lang.{DataDecl, Expression, Program, ProgramSegment, Statement}
import util.SEOps

object validate {
  val Keywords = List(
    "if", "else", "word1", "word2", "word4", "word8", "float4", "float8",
    "align", "data", "import", "export", "neg", "abs", "sign", "trunc", "round"
  )

  type Rez[T] = Either[String, T]

  def validate(prog: Program): Rez[Unit] = for {
    _ <- checkScope(prog)
    _ <- ensureSizes(prog)
    _ <- validUsages(prog)
  } yield ()

  def checkName(name: String): Rez[Unit] = {
    if (Keywords.contains(name)) {
      Left(s"Illegal use of keyword '$name' as identifier")
    } else Right(())
  }

  def checkScope(prog: Program): Rez[Unit] = {
    for {
      imports <- collectImports(prog)
      datum <- collectDataNames(prog, imports)
      procs <- collectProcNames(prog, imports ++ datum)
      syms = datum ++ imports
      _ <- prog.segments.map {
        case db: DataBlock => checkScope(db, syms)
        case pd: ProcDefn => checkScope(pd, syms, procs)
        case ProgramSegment.SymbolExport(name) =>
          if (datum.contains(name) || procs.contains(name)) Right(()) else {
            Left(s"Cannot export symbol $name which is not defined ")
          }
        case _ => Right(())
      }.sequence
    } yield ()
  }

  def collectImports(prog: Program): Rez[Set[String]] = {
    val all = prog.segments.collect {
      case ProgramSegment.SymbolImport(name) => name
    }

    if (all.toSet.size < all.length) {
      Left("Symbol imports conflict")
    } else {
      all.map(checkName).sequence.map(_ => all.toSet)
    }
  }

  def collectDataNames(prog: Program, bad: Set[String]): Rez[Set[String]] = {
    prog.segments.collect {
      case db: DataBlock => collectNames(db)
    }.sequence.flatMap { ssn =>
      val sn = ssn.flatten
      if (sn.toSet.size < sn.length) {
        Left("An identifier defined in one data block was redefined in another")
      } else if (sn.exists(s => bad.contains(s))) {
        Left("A symbol is defined which has already been imported")
      } else Right(sn.toSet)
    }
  }

  def collectNames(db: DataBlock): Rez[Seq[String]] = {
    val nms = db.decls.map(d => checkName(d.name).map(_ => d.name)).sequence

    nms.flatMap { names =>
      if (names.toSet.size < names.length) {
        Left("Illegal re-use of identifier in data block")
      } else Right(names)
    }
  }

  def collectProcNames(prog: Program, bad: Set[String]): Rez[Set[String]] = {
    val all = prog.segments.collect {
      case ProcDefn(name, _, _) => name
    }

    if (all.toSet.size < all.length) {
      Left("Procedure names are not distinct")
    } else {
      all.map(checkName)
        .sequence
        .map(_ => all.toSet)
        .flatMap { setz =>
          if (setz.exists(s => bad.contains(s))) {
            Left("Procedure name conflicts with datum name or imported symbol")
          } else Right(setz)
        }
    }
  }

  def checkScope(db: DataBlock, valid: Set[String]): Rez[Unit] = {
    db.decls
      .flatMap(d => d.value.map(e => checkScope(e, valid - d.name, Set.empty, Set.empty)))
      .sequence
      .map(_ => ())
  }

  def checkScope(proc: ProcDefn, datum: Set[String], procs: Set[String]): Rez[Unit] = {
    if (proc.params.exists(t => datum.contains(t._1) || procs.contains(t._1))) {
      Left(s"Parameter to procedure ${proc.name} conflicts with extant name")
    } else {
      val nd = datum union proc.params.map(_._1).toSet

      checkScope(proc.body, nd, procs, Set.empty).map(_ => ())
    }
  }

  def checkScope(stmt: Statement, datum: Set[String],
                 procs: Set[String], labels: Set[String]): Rez[Set[String]] = stmt match {
    case Statement.VarDecl(_, names@_*) =>
      if (names.exists(s => datum.contains(s) || procs.contains(s) || labels.contains(s))) {
        Left("Declared variable conflicts with extant name")
      } else Right(labels ++ names.toSet)
    case Statement.Assn(name, value) =>
      if (datum.contains(name) || labels.contains(name)) {
        checkScope(value, datum, procs, labels).map(_ => labels)
      } else Left("Attempting to assign to non-existing variable")
    case Statement.Write(_, pos, value, _) =>
      checkScope(pos, datum, procs, labels)
        .flatMap(_ => checkScope(value, datum, procs, labels))
        .map(_ => labels)
    case Statement.IfStmt(lhs, _, rhs, exec, elseExec) =>
      for {
        _ <- checkScope(lhs, datum, procs, labels)
        _ <- checkScope(rhs, datum, procs, labels)
        _ <- checkScope(exec, datum, procs, labels)
        _ <- elseExec.map(checkScope(_, datum, procs, labels)).getOrElse(Right(Set.empty))
      } yield labels
    case Statement.LocalLabel(name) =>
      Right(labels + name)
    case Statement.Goto(label) =>
      if (labels.contains(label)) Right(labels) else Left(s"Cannot goto non-existent label $label")
    case Statement.Jump(proc, args@_*) =>
      checkScope(proc, datum, procs, labels)
        .flatMap(_ => args.map(checkScope(_, datum, procs, labels)).sequence.map(_ => labels))
    case Statement.Call(results, proc, args@_*) =>
      if (results.exists(s => !labels.contains(s))) {
        Left("Attempting to assign call result to non-existing or invalid variable")
      } else {
        checkScope(proc, datum, procs, labels)
          .flatMap(_ => args.map(checkScope(_, datum, procs, labels)).sequence.map(_ => labels))
      }
    case Statement.Return(results@_*) =>
      results.map(checkScope(_, datum, procs, labels)).sequence.map(_ => labels)
    case Statement.Block(stmts@_*) =>
      val bc: Rez[Set[String]] = Right(labels)
      stmts.foldLeft(bc) { (cls, st) =>
        cls match {
          case Left(value) => Left(value)
          case Right(value) => checkScope(st, datum, procs, value)
        }
      }.map(_ => labels)
    case _ => Right(labels)
  }

  def checkScope(expr: Expression, datum: Set[String],
                 procs: Set[String], labels: Set[String]): Rez[Unit] = expr match {
    case Expression.ID(name) =>
      if (procs.contains(name) || datum.contains(name) || labels.contains(name)) {
        Right(())
      } else Left(s"Unknown identifier $name")
    case Expression.Read(_, pos, _) => checkScope(pos, datum, procs, labels)
    case Expression.InfixOp(lhs, _, rhs) =>
      checkScope(lhs, datum, procs, labels).flatMap(_ => checkScope(rhs, datum, procs, labels))
    case Expression.PrefixOp(_, target) =>
      checkScope(target, datum, procs, labels)
    case Expression.PrimOp(_, args) =>
      args.map(checkScope(_, datum, procs, labels)).sequence.map(_ => ())
    case _ => Right(())
  }

  def ensureSize(decl: DataDecl): Rez[Unit] = {
    if (decl.count == 0 || decl.count < decl.value.length) Right(()) else {
      Left(s"Initialization size cannot be greater than allocation size (for datum ${decl.name})")
    }
  }
  def ensureSizes(prog: Program): Rez[Unit] = {
    prog.segments.map {
      case DataBlock(decls@_*) => decls.map(ensureSize).sequence
      case _ => Right(Seq.empty)
    }.sequence.map(_ => ())
  }

  def validUsages(statement: Statement): Rez[Unit] = statement match {
    case Statement.Skip => Right(())
    case Statement.VarDecl(_, names@_*) => if (names.isEmpty) {
      Left("Variable declarations must declare at least one var")
    } else Right(())
    case Statement.Assn(_, _) => Right(())
    case Statement.Write(_, _, _, _) => Right(())
    case Statement.IfStmt(_, _, _, exec, elseExec) =>
      validUsages(exec).flatMap(_ => validUsages(elseExec.getOrElse(Statement.Skip)))
    case Statement.LocalLabel(_) => Right(())
    case Statement.Goto(_) => Right(())
    case Statement.Jump(_, _) => Right(())
    case Statement.Call(results, _, _) => if (results.length > 2) {
      Left("Procedures returning more than 2 values are not supported")
    } else Right(())
    case Statement.Return(results@_*) => if (results.length > 2) {
      Left("Procedures returning more than 2 values are not supported")
    } else Right(())
    case Statement.Block(stmts@_*) => stmts.map(validUsages).sequence.map(_ => ())
  }

  def validUsages(prog: Program): Rez[Unit] = prog.segments.collect {
    case ProcDefn(_, _, body) => validUsages(body)
  }.sequence.map(_ => ())
}

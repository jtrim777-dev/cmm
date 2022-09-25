package dev.jtrim777.cmm
package compile.common

import lang.ProgramSegment.{DataBlock, ProcDefn}
import lang.{DataDecl, Expression, Program, ProgramSegment, Statement}

import collection.immutable.Seq
import Phase.{Printer, check, phase}

import cats.effect.IO
import cats.effect.IO.pure
import cats.implicits.toTraverseOps
import dev.jtrim777.cmm.Phase

object Validate extends Phase.Group[IO, Program, Program]("validate") {
  
  override def body: Phase[IO, Program, Program] = {
    check(checkScope) *>
      check(ensureSizes) *>
      check(validateUsages)
  }
  
  // MARK: Begin sub-phases
  def checkScope: Phase[IO, Program, Unit] = phase("checkScope") { (prog, log) =>
     for {
      imports <- collectImports(prog)
      datum <- collectDataNames(prog, imports)
      procs <- collectProcNames(prog, imports ++ datum)
      syms = datum ++ imports
      _ <- prog.dataBlocks.map(checkScope(_, syms)).sequence
      _ <- prog.procedures.map(checkScope(_, syms, procs)).sequence
      _ <- prog.ports.map {
        case ProgramSegment.SymbolExport(name) =>
          if (datum.contains(name) || procs.contains(name)) success else {
            raise(s"Cannot export symbol $name which is not defined ")
          }
        case _ => success
      }.sequence
    } yield ()
  }

  def ensureSizes: Phase[IO, Program, Unit] = phase("ensureSizes") { (prog, log) =>
    prog.dataBlocks.map { db =>
      db.decls.map(ensureSize).sequence
    }.sequence.void
  }

  def validateUsages: Phase[IO, Program, Unit] = phase("validateUsages") { (prog, log) =>
    prog.procedures.map(proc => validUsages(proc.body)).sequence.void
  } 
  
  // MARK: Begin helpers
  private val Keywords = List(
    "if", "else", "word1", "word2", "word4", "word8", "float4", "float8",
    "align", "data", "import", "export", "neg", "abs", "sign", "trunc", "round"
  )
  
  case class ProgramValidationException(message: String) extends Exception(message)
  def raise(message: String): IO[Nothing] = IO.raiseError(ProgramValidationException(message))
  
  def success: IO[Unit] = pure(())

  def checkName(name: String): IO[Unit] = {
    if (Keywords.contains(name)) {
      raise(s"Illegal use of keyword '$name' as identifier")
    } else success
  }

  def collectImports(prog: Program): IO[Set[String]] = {
    val all = prog.ports.collect {
      case ProgramSegment.SymbolImport(name) => name
    }

    if (all.toSet.size < all.length) {
      raise("Symbol imports conflict")
    } else {
      all.map(checkName).sequence.map(_ => all.toSet)
    }
  }

  def collectDataNames(prog: Program, bad: Set[String]): IO[Set[String]] = {
    prog.dataBlocks.map(collectNames).sequence.flatMap { ssn =>
      val sn = ssn.flatten
      if (sn.toSet.size < sn.length) {
        raise("An identifier defined in one data block was redefined in another")
      } else if (sn.exists(s => bad.contains(s))) {
        raise("A symbol is defined which has already been imported")
      } else pure(sn.toSet)
    }
  }

  def collectNames(db: DataBlock): IO[Seq[String]] = {
    val nms = db.decls.map(d => checkName(d.name).map(_ => d.name)).sequence

    nms.flatMap { names =>
      if (names.toSet.size < names.length) {
        raise("Illegal re-use of identifier in data block")
      } else pure(names)
    }
  }

  def collectProcNames(prog: Program, bad: Set[String]): IO[Set[String]] = {
    val all = prog.procedures.map(_.name)

    if (all.toSet.size < all.length) {
      raise("Procedure names are not distinct")
    } else {
      all.map(checkName)
        .sequence
        .map(_ => all.toSet)
        .flatMap { setz =>
          if (setz.exists(s => bad.contains(s))) {
            raise("Procedure name conflicts with datum name or imported symbol")
          } else pure(setz)
        }
    }
  }

  def checkScope(db: DataBlock, valid: Set[String]): IO[Unit] = {
    db.decls
      .flatMap(d => d.value.map(e => checkScope(e, valid - d.name, Set.empty, Set.empty)))
      .sequence
      .map(_ => ())
  }

  def checkScope(proc: ProcDefn, datum: Set[String], procs: Set[String]): IO[Unit] = {
    if (proc.params.exists(t => datum.contains(t._1) || procs.contains(t._1))) {
      raise(s"Parameter to procedure ${proc.name} conflicts with extant name")
    } else {
      val nd = datum union proc.params.map(_._1).toSet

      checkScope(proc.body, nd, procs, Set.empty).map(_ => ())
    }
  }

  def checkScope(stmt: Statement, datum: Set[String],
                 procs: Set[String], labels: Set[String]): IO[Set[String]] = stmt match {
    case Statement.VarDecl(_, names@_*) =>
      if (names.exists(s => datum.contains(s) || procs.contains(s) || labels.contains(s))) {
        raise("Declared variable conflicts with extant name")
      } else pure(labels ++ names.toSet)
    case Statement.Assn(name, value) =>
      if (datum.contains(name) || labels.contains(name)) {
        checkScope(value, datum, procs, labels).map(_ => labels)
      } else raise("Attempting to assign to non-existing variable")
    case Statement.Write(_, pos, value, _) =>
      checkScope(pos, datum, procs, labels)
        .flatMap(_ => checkScope(value, datum, procs, labels))
        .map(_ => labels)
    case Statement.IfStmt(cond, exec, elseExec) =>
      for {
        _ <- checkScope(cond, datum, procs, labels)
        _ <- checkScope(exec, datum, procs, labels)
        _ <- elseExec.map(checkScope(_, datum, procs, labels)).getOrElse(pure(Set.empty))
      } yield labels
    case Statement.LocalLabel(name) =>
      pure(labels + name)
    case Statement.Goto(label) =>
      if (labels.contains(label)) pure(labels) else raise(s"Cannot goto non-existent label $label")
    case Statement.Jump(proc, args@_*) =>
      checkScope(proc, datum, procs, labels)
        .flatMap(_ => args.map(checkScope(_, datum, procs, labels)).sequence.map(_ => labels))
    case Statement.Call(results, proc, args@_*) =>
      if (results.exists(s => !labels.contains(s))) {
        raise("Attempting to assign call result to non-existing or invalid variable")
      } else {
        checkScope(proc, datum, procs, labels)
          .flatMap(_ => args.map(checkScope(_, datum, procs, labels)).sequence.map(_ => labels))
      }
    case Statement.Return(results@_*) =>
      results.map(checkScope(_, datum, procs, labels)).sequence.map(_ => labels)
    case Statement.Block(stmts) =>
      val bc: IO[Set[String]] = pure(labels)
      stmts.foldLeft(bc) { (cls, st) =>
        cls.flatMap(value => checkScope(st, datum, procs, value))
      }.map(_ => labels)
    case _ => pure(labels)
  }

  def checkScope(expr: Expression, datum: Set[String],
                 procs: Set[String], labels: Set[String]): IO[Unit] = expr match {
    case Expression.ID(name) =>
      if (procs.contains(name) || datum.contains(name) || labels.contains(name)) {
        success
      } else raise(s"Unknown identifier $name")
    case Expression.Read(_, pos, _) => checkScope(pos, datum, procs, labels)
    case Expression.Operation(_, args) =>
      args.map(checkScope(_, datum, procs, labels)).sequence.map(_ => ())
    case _ => success
  }

  def ensureSize(decl: DataDecl): IO[Unit] = {
    if (decl.count == 0 || decl.count < decl.value.length) success else {
      raise(s"Initialization size cannot be greater than allocation size (for datum ${decl.name})")
    }
  }
  
  def validUsages(statement: Statement): IO[Unit] = statement match {
    case Statement.Skip => success
    case Statement.VarDecl(_, names@_*) => if (names.isEmpty) {
      raise("Variable declarations must declare at least one var")
    } else success
    case Statement.Assn(_, _) => success
    case Statement.Write(_, _, _, _) => success
    case Statement.IfStmt(_, exec, elseExec) =>
      validUsages(exec).flatMap(_ => validUsages(elseExec.getOrElse(Statement.Skip)))
    case Statement.LocalLabel(_) => success
    case Statement.Goto(_) => success
    case Statement.Jump(_, _) => success
    case Statement.Call(results, _, _) => if (results.length > 2) {
      raise("Procedures returning more than 2 values are not supported")
    } else success
    case Statement.Return(results@_*) => if (results.length > 2) {
      raise("Procedures returning more than 2 values are not supported")
    } else success
    case Statement.Block(stmts) => stmts.map(validUsages).sequence.map(_ => ())
  }

  
}

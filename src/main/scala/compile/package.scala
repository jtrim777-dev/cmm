package dev.jtrim777.cmm

import isa.ISA
import lang.Program

import cats.effect.IO

import java.util.UUID

package object compile {
  type CompilePhase[Arch <: ISA] = Phase.Group[IO, Program, CompiledProgram[Arch]]

  case class CompilationError(region: String, message: String) extends Exception(s"Error compiling $region: $message")

  def raise(region: String, message: String): IO[Nothing] = IO.raiseError(CompilationError(region, message))
  def verify(cond: => Boolean, region: String, message: => String): IO[Unit] = IO.whenA(!cond)(raise(region, message))

  def genID(): IO[String] = IO(UUID.randomUUID().toString.replace('-', '_'))
}

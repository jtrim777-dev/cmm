package dev.jtrim777.cmm
package compile

import isa.ISA
import lang.Program

import cats.effect.IO

class Compile[Arch <: ISA](val archCompiler: CompilePhase[Arch]) extends Phase.Group[IO, Program, CompiledProgram[Arch]]("compile") {
  override def body: Phase[IO, Program, CompiledProgram[Arch]] = {
    common.Validate *> common.Prepare *> archCompiler
  }
}

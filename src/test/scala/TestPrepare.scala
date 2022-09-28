package dev.jtrim777.cmm

import lang.Program

import cats.effect.{ExitCode, IO, IOApp}

object TestPrepare extends IOApp {

  override def run(args: List[String]): IO[ExitCode] = for {
    src <- TestUtils.loadExampleProgram(args.headOption.flatMap(_.toIntOption).getOrElse(1))
    result <- Execution.run(src)
    _ <- IO(pprint.pprintln(result))
  } yield ExitCode.Success

  val Execution: Phase[IO, String, Program] = parse.Parse *> compile.common.Validate *> compile.common.Prepare
}

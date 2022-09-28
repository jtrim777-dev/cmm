package dev.jtrim777.cmm

import cats.effect.{ExitCode, IO, IOApp}

import java.nio.file.{Files, Paths}
import cats.implicits.toTraverseOps
import dev.jtrim777.cmm.TestUtils.loadExampleProgram

object TestParser extends IOApp {
  val exampleCount = 3

  override def run(args: List[String]): IO[ExitCode] = {
//    (1 to exampleCount).toList.map(testExample).sequence.as(ExitCode.Success)
    testExample(args.headOption.flatMap(_.toIntOption).getOrElse(1)).as(ExitCode.Success)
  }

  def testExample(index: Int): IO[Unit] = {
    for {
      exampleProgram <- loadExampleProgram(index)
      result <- parse.Parse.run(exampleProgram)
      _ <- IO(pprint.pprintln(result))
    } yield ()
  }
}

package dev.jtrim777.cmm

import cats.effect.{ExitCode, IO, IOApp}
import java.nio.file.{Paths, Files}
import cats.implicits.toTraverseOps

object TestParser extends IOApp {
  val exampleCount = 3

  override def run(args: List[String]): IO[ExitCode] = {
    (1 to exampleCount).toList.map(testExample).sequence.as(ExitCode.Success)
  }

  def testExample(index: Int): IO[Unit] = {
    for {
      exampleProgram <- loadExampleProgram(index)
      result <- parse.Parse.run(exampleProgram)
      _ <- IO(pprint.pprintln(result))
    } yield ()
  }

  def loadExampleProgram(index: Int): IO[String] = {
    for {
      filePath <- IO(Paths.get(s"/Users/jaketrimble/Documents/cmm/src/test/resources/Example$index.cmm"))
      content <- IO(Files.readString(filePath))
    } yield content
  }
}

package dev.jtrim777.cmm

import cats.effect.IO

import java.nio.file.{Files, Paths}

object TestUtils {
  def loadExampleProgram(index: Int): IO[String] = {
    for {
      filePath <- IO(Paths.get(s"/Users/jaketrimble/Documents/cmm/src/test/resources/Example$index.cmm"))
      content <- IO(Files.readString(filePath))
    } yield content
  }
}

package dev.jtrim777.cmm

import lang.Program

import cats.effect.IO

import scala.util.{Failure, Success}

package object parse {
  val Parse: Phase[IO, String, Program] = Phase.phase("parse") { (text, _) =>
    val impl = new ParserImpl(text)

    IO(impl.Prog.run()) flatMap {
      case Success(value) => IO.pure(value)
      case Failure(exception) => IO.raiseError(exception)
    }
  }
}

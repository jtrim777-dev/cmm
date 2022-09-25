package dev.jtrim777.cmm

import lang.Program

import cats.effect.IO
import org.parboiled2.{ErrorFormatter, ParseError}

import scala.util.{Failure, Success}

package object parse {
  case class ParseFailure(message: String) extends Exception(message)

  val Parse: Phase[IO, String, Program] = Phase.phase("parse") { (text, _) =>
    val impl = new ParserImpl(text)

    IO(impl.Prog.run()) flatMap {
      case Success(value) => IO.pure(value)
      case Failure(exception:ParseError) => IO.raiseError(ParseFailure(impl.formatError(exception, new ErrorFormatter(showTraces = true))))
      case Failure(otherex) => IO.raiseError(otherex)
    }
  }
}

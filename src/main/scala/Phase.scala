package dev.jtrim777.cmm

import cats.data.Kleisli
import cats.effect.Sync
import cats.syntax.flatMap._
import cats.syntax.functor._

sealed abstract class Phase[F[_] : Sync, I, O](val name: String) {
  private[Phase] def execute(input: I, depth: Int, order: Int): F[O]

  def andThen[OO](next: Phase[F, O, OO]): Phase[F, I, OO] = Phase.Chain("", this, next)

  def *>[OO](next: Phase[F, O, OO]): Phase[F, I, OO] = this.andThen(next)

  private def logMethod(depth: Int, order: Int): Phase.Printer[F] = {
    ???
  }
}

object Phase {
  type Printer[F[_] : Sync] = Kleisli[F, String, Unit]

  abstract class Node[F[_] : Sync, I, O](name: String) extends Phase[F, I, O](name) {
    override private[Phase] def execute(input: I, depth: Int, order: Int): F[O] = execute(input, logMethod(depth, order))

    def execute(input: I, log: Printer[F]): F[O]
  }

  case class Chain[F[_] : Sync, I, M, O](override val name: String,
                                         head: Phase[F, I, M], tail: Phase[F, M, O]) extends Phase[F, I, O](name) {
    override private[Phase] def execute(input: I, depth: Int, order: Int): F[O] = {
      for {
        interm <- head.execute(input, depth, order)
        rez <- tail.execute(interm, depth, order + 1)
      } yield rez
    }

    override def andThen[OO](next: Phase[F, O, OO]): Phase[F, I, OO] = {
      this.copy(tail = tail.andThen(next))
    }
  }

  abstract class Group[F[_] : Sync, I, O](name: String) extends Phase[F, I, O](name) {
    override private[Phase] def execute(input: I, depth: Int, order: Int): F[O] = {
      for {
        o1 <- preExecute(input, logMethod(depth, order))
        o2 <- body.execute(o1, depth + 1, 0)
        o3 <- postExecute(o2, logMethod(depth, order))
      } yield o3
    }

    def preExecute(input: I, log: Printer[F]): F[I] = implicitly[Sync[F]].pure(input)

    def body: Phase[F, I, O]

    def postExecute(result: O, log: Printer[F]): F[O] = implicitly[Sync[F]].pure(result)
  }

  def check[F[_] : Sync, I](behavior: Phase[F, I, Unit]): Phase[F, I, I] = new Phase[F, I, I](behavior.name) {
    override private[Phase] def execute(input: I, depth: Int, order: Int): F[I] =
      behavior.execute(input, depth, order).map(_ => input)
  }

  def phase[F[_] : Sync, I, O](name: String)(body: (I, Printer[F]) => F[O]): Phase.Node[F, I, O] = new Node[F, I, O](name) {
    override def execute(input: I, log: Printer[F]): F[O] = body(input, log)
  }
}

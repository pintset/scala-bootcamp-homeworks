package client.strategies

import cats.effect.Sync
import cats.effect.std.Console
import client.types.{GameStrategy, Move}
import common.domain.AttemptResult

object ConsoleStrategy {
  def apply[F[_]: Sync: Console]: GameStrategy[F] = common.Console.readInt
  def move[F[_]: Sync: Console](guess: Int => F[AttemptResult]): Move[F] = Move(ConsoleStrategy[F], guess)
}
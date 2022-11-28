package client.strategies

import cats.effect.Sync
import cats.effect.std.Console
import client.types.{GameStrategy, Move}
import common.domain.AttemptResult

object ConsoleStrategy {
  def apply[F[_]: Sync: Console]: GameStrategy[F, String] = Console[F].readLine
  def move[F[_]: Sync: Console](guess: String => F[AttemptResult[String]]): Move[F, String] = Move(ConsoleStrategy[F], guess)
}
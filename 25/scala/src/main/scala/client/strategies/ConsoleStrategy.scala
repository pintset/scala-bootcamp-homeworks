package client.strategies

import cats.effect.Sync
import client.types.{GameStrategy, Move}
import common.domain.AttemptResult
import effects.Console

object ConsoleStrategy {
  def apply[F[_] : Sync]: GameStrategy[F] = Console[F].getInt
  def move[F[_] : Sync](guess: Int => F[AttemptResult]): Move[F] = Move(ConsoleStrategy[F], guess)
}
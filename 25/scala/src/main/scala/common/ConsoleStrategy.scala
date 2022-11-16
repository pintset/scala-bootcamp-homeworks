package common

import cats.effect.Sync
import common.domain.{AttemptResult, Move}
import effects.Console

object ConsoleStrategy {
  def apply[F[_]: Sync]: GameStrategy[F] = Console[F].getInt
  def move[F[_] : Sync](guess: Int => F[AttemptResult]): Move[F] = Move(ConsoleStrategy[F], guess)
}

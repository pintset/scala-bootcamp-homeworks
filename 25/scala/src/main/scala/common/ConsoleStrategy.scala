package common

import cats.Applicative
import cats.effect.Sync
import common.domain.AttemptResult
import effects.Console
import http.GuessClient.Move

object ConsoleStrategy {
  def apply[F[_]: Sync]: GameStrategy[F] = Console[F].getInt
  def move[F[_] : Sync](guess: Int => F[AttemptResult]): Move[F] = Move(ConsoleStrategy[F], guess)
}

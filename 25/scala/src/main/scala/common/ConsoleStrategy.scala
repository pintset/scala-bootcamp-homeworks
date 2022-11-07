package common

import cats.effect.Sync
import effects.Console

object ConsoleStrategy {
  def apply[F[_]: Sync]: GameStrategy[F] = _ => Console[F].getInt
}

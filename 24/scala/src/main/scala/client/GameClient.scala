package client

import cats.effect.Concurrent
import common.domain.{AttemptResult, NewGame}

import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.effect.syntax.concurrent._

object GameClient {
  def apply[F[_] : Concurrent](game: GameService[F]): NewGame => F[Int => F[AttemptResult]] =
    settings =>
      game.start(settings)
        .memoize
        .map { gameIdF =>
          guess => gameIdF >>= { gameId => game.guess(gameId, guess) }
        }
}
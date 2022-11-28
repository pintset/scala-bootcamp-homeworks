package client

import cats.effect.Concurrent
import common.domain.{AttemptResult, NewGame}

import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.effect.syntax.concurrent._

object GameClient {
  def apply[F[_] : Concurrent, A](game: GameService[F, A]): NewGame[A] => F[A => F[AttemptResult[A]]] =
    settings =>
      game.start(settings)
        .memoize
        .map { gameIdF =>
          guess => gameIdF >>= { gameId => game.guess(gameId, guess) }
        }
}
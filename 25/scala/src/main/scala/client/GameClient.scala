package client

import cats.effect.Concurrent
import common.domain.{AttemptResult, NewGame}

import cats.syntax.flatMap._
import cats.syntax.functor._

object GameClient {
  def apply[F[_] : Concurrent](game: GameService[F]): NewGame => F[Int => F[AttemptResult]] = {
    implicit class ConcurrentOps[A](fa: F[A]) {
      def memoize: F[F[A]] = Concurrent.memoize(fa)
    }

    settings =>
      game.start(settings)
        .memoize
        .map { gameIdF =>
          guess => gameIdF >>= { gameId => game.guess(gameId, guess) }
        }
  }
}

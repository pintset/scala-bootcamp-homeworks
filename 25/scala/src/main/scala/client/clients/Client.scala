package client.clients

import cats.effect.Concurrent
import common.domain.{AttemptResult, GameId, NewGame}

import cats.syntax.flatMap._
import cats.syntax.functor._

trait Api[F[_]] {
  def start(settings: NewGame): F[GameId]
  def guess(gameId: GameId, guess: Int): F[AttemptResult]
}

object CreateClient {
  def apply[F[_]: Concurrent](client: Api[F]): NewGame => F[Int => F[AttemptResult]] = {
    implicit class ConcurrentOps[A](fa: F[A]) {
      def memoize: F[F[A]] = Concurrent.memoize(fa)
    }

    settings =>
      client.start(settings)
        .memoize
        .map { gameIdF =>
            guess => gameIdF >>= { gameId => client.guess(gameId, guess) }
        }
  }
}
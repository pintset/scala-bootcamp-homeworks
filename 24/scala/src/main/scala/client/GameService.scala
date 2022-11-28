package client

import common.domain.{AttemptResult, GameId, NewGame}

trait GameService[F[_], A] {
  def start(settings: NewGame[A]): F[GameId]
  def guess(gameId: GameId, guess: A): F[AttemptResult[A]]
}

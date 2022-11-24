package client

import common.domain.{AttemptResult, GameId, NewGame}

trait GameService[F[_]] {
  def start(settings: NewGame): F[GameId]
  def guess(gameId: GameId, guess: Int): F[AttemptResult]
}

package client

import common.domain.{AttemptResult, NewGame}

object types {
  final case class Move[F[_]](getNext: F[Int], guess: Int => F[AttemptResult])

  type GameClient[F[_]] = Int => F[AttemptResult]
  type GameStrategy[F[_]] = F[Int]
  type Game[F[_]] = NewGame => F[AttemptResult]
}

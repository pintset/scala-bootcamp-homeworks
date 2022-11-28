package client

import common.domain.{AttemptResult, NewGame}

object types {
  final case class Move[F[_], A](getNext: F[A], guess: A => F[AttemptResult[A]])

  type GameClient[F[_], A] = A => F[AttemptResult[A]]
  type GameStrategy[F[_], A] = F[A]
  type Game[F[_], A] = NewGame[A] => F[AttemptResult[A]]
}

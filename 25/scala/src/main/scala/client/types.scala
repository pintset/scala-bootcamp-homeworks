package client

import common.domain.{AttemptResult, NewGame}

object types {
  final case class Move[F[_]](getNext: F[Int], guess: Int => F[AttemptResult])

  // guess
  type GameClient[F[_]] = Int => F[AttemptResult]
  // type GameClient[F[_]] = Kleisli[F, Int, AttemptResult]

  // getNext (F[Int] in particular)
  type GameStrategy[F[_]] = F[Int]
  // type GameStrategy[F[_]] = Kleisli[F, Option[AttemptResult], Int]

  type Game[F[_]] = NewGame => F[AttemptResult]
  // type Game[F[_]] = Kleisli[F, NewGame, AttemptResult]
}

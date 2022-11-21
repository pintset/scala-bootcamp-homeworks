package client

import common.domain.{AttemptResult, NewGame}

object types {
  final case class Move[F[_]](getNext: F[Int], guess: Int => F[AttemptResult])

  // guess
  type Client[F[_]] = Int => F[AttemptResult]
  // type Client[F[_]] = Kleisli[F, Int, AttemptResult]

  // getNext (F[Int] in particular)
  type GameStrategy[F[_]] = F[Int]
  // type GameStrategy[F[_]] = Kleisli[F, Option[AttemptResult], Int]

  type Game[F[_]] = NewGame => F[AttemptResult]
  // type TheGame[F[_]] = Kleisli[F, NewGame, AttemptResult]
}

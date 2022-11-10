import cats.data.Kleisli
import common.domain.{AttemptResult, NewGame}

package object common {
  // guess
  type Client[F[_]] = Int => F[AttemptResult]
  // type Client[F[_]] = Kleisli[F, Int, AttemptResult]


  // getNext (F[Int] in particular)
  type GameStrategy[F[_]] = Option[AttemptResult] => F[Int]
  // type GameStrategy[F[_]] = Kleisli[F, Option[AttemptResult], Int]

  type TheGame[F[_]] = NewGame => F[AttemptResult]
  // type TheGame[F[_]] = Kleisli[F, NewGame, AttemptResult]
}
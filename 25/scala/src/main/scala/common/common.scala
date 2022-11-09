import common.domain.AttemptResult

package object common {
  // guess
  type Client[F[_]] = Int => F[AttemptResult]

  // getNext (F[Int] in particular)
  type GameStrategy[F[_]] = Option[AttemptResult] => F[Int]
}

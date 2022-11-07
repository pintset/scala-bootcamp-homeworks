package common

import common.domain.AttemptResult

trait GameStrategy[F[_]] {
  def getNext(prevAttemptResultOpt: Option[AttemptResult]): F[Int]
}
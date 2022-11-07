package common

import common.domain.AttemptResult

trait Client[F[_]] {
  def guess(number: Int): F[AttemptResult]
}

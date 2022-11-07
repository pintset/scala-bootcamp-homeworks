package common

import cats.Monad
import cats.data.StateT
import common.domain.{AttemptResult, Greater, Lower}

object BotStrategy {
  final case class MinMax(min: Int, max: Int)

  def apply[F[_]: Monad]: GameStrategy[StateT[F, MinMax, *]] = prevAttemptResultOpt => {
    def strategy(v: MinMax): Int = v.min + (v.max - v.min) / 2

    // Fold?
    // Strategy prev вычисляется всегда?
    // nextState - реально фолд. Гейм луп потенциально тоже фолд, кстати - там же стейт передаётся :)
    def nextMinMax(prev: MinMax, prevAttemptResultOpt: Option[AttemptResult]): MinMax =
      prevAttemptResultOpt.map {
        case Greater(_) => MinMax(prev.min, strategy(prev))
        case Lower(_) => MinMax(strategy(prev), prev.max)
        case _ => prev
      }.getOrElse(prev)

    StateT { prevMinMax =>
      val minMax = nextMinMax(prevMinMax, prevAttemptResultOpt)
      val nextGuess: Int = strategy(minMax)
      Monad[F].pure(minMax, nextGuess)
    }
  }
}
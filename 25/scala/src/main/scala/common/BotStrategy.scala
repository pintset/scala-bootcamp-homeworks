package common

import cats.Monad
import cats.data.{State, StateT}
import common.domain.{AttemptResult, Greater, Lower}

object BotStrategy {
  final case class MinMax(min: Int, max: Int)

  def apply[F[_]: Monad]: GameStrategy[StateT[F, MinMax, *]] = prevAttemptResultOpt => {
    def strategy(v: MinMax): Int = v.min + (v.max - v.min) / 2

    // Fold?
    // Strategy prev вычисляется всегда?
    // nextState - реально фолд. Гейм луп потенциально тоже фолд, кстати - там же стейт передаётся :)
    // nextMinMax - это State.modify
    def nextMinMax(prev: MinMax, prevAttemptResultOpt: Option[AttemptResult]): MinMax =
      prevAttemptResultOpt.map {
        case Greater(_) => MinMax(prev.min, strategy(prev))
        case Lower(_) => MinMax(strategy(prev), prev.max)
        case _ => prev
      }.getOrElse(prev)

    def nextMinMax2(prevAttemptResultOpt: Option[AttemptResult]) = StateT.modify[F, MinMax] { prev =>
      prevAttemptResultOpt.map {
        case Greater(_) => MinMax(prev.min, strategy(prev))
        case Lower(_) => MinMax(strategy(prev), prev.max)
        case _ => prev
      }.getOrElse(prev)
    }

    def nextMinMax3(prevAttemptResultOpt: Option[AttemptResult]) =
      prevAttemptResultOpt.map {
        case Greater(_)   => StateT.modify[F, MinMax] { prev => MinMax(prev.min, strategy(prev)) }
        case Lower(_)     => StateT.modify[F, MinMax] { prev => MinMax(strategy(prev), prev.max) }
        case _            => StateT.modify[F, MinMax](identity)
      }.getOrElse           (StateT.modify[F, MinMax](identity))

    // Использовать стейт только там где надо
    StateT { prevMinMax =>
      // MinMax - это стейт
      val minMax = nextMinMax(prevMinMax, prevAttemptResultOpt)
      val nextGuess: Int = strategy(minMax)
      Monad[F].pure(minMax, nextGuess)
    }
  }
}